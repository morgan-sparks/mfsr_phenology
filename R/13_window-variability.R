# =============================================================================
# 13_window-variability.R  (exploratory; follow-up to the CJFAS revision)
#
# Does how temperature varies within a window, not just its mean, help explain
# spawn timing?
#
# The final model uses one number per reach-year: the mean modeled
# temperature, 18 May - 16 Jul (T). Here we describe each reach-year's daily
# temperature series in two fixed-date windows with several shape metrics, and
# ask whether any of them adds to the final model.
#
#   H1  Metrics per reach-year, and how redundant they are with T
#   H2  Each metric added to the final model (ML; with and without Big Creek)
#   H3  Holding-window cooling rate split into its between-reach and
#       within-reach parts (as T is split into T_bar and T_dev)
#
# Outputs (tables/revision/, plots/revision/):
#   H1_window_metrics.csv, H1_metric_correlations.csv
#   H2_metric_added_to_final.csv
#   H3_cooling_split.csv, H3_cooling_rate.png
# =============================================================================

source(here::here("R", "revision_setup.R"))   # temps, model_data(), FINAL_FORMULA, ctl, std_doy()
library(lme4)
theme_set(theme_mfsr())

# Fixed-date windows (non-leap DOY, as everywhere in the revision)
WINDOWS <- list(
  idx  = c(lo = 138, hi = 197),   # 18 May - 16 Jul: the thermal index window
  hold = c(lo = 198, hi = 227)    # 17 Jul - 15 Aug: adults holding in tributaries
)

# -----------------------------------------------------------------------------
# H1. Shape metrics for one window, one row per reach-year
#   mean   : window mean (for "idx" this equals T in the model)
#   sd     : SD of daily means
#   slope  : linear trend, deg C per 10 d (positive = warming, negative = cooling)
#   max7   : warmest 7-day running mean
#   range  : max - min daily mean
#   flash  : mean absolute day-to-day change (how "jumpy" the series is)
# -----------------------------------------------------------------------------
window_metrics <- function(lo, hi, tag) {
  temps |>
    filter(doy >= lo, doy <= hi) |>
    arrange(COMID, year, doy) |>
    group_by(COMID, year) |>
    summarise(
      mean  = mean(temp),
      sd    = sd(temp),
      slope = coef(lm(temp ~ doy))[2] * 10,
      max7  = max(stats::filter(temp, rep(1 / 7, 7), sides = 2), na.rm = TRUE),
      range = diff(range(temp)),
      flash = mean(abs(diff(temp))),
      .groups = "drop"
    ) |>
    rename_with(~ paste0(tag, "_", .x), -c(COMID, year))
}

ry_metrics <- imap(WINDOWS, \(w, tag) window_metrics(w["lo"], w["hi"], tag)) |>
  reduce(full_join, by = c("COMID", "year")) |>
  mutate(key = paste(COMID, year, sep = "_"))

# Attach to the model data (one row per redd). model_data() supplies T, T_bar
# and T_dev; key is the reach-year identifier.
d <- model_data() |>
  left_join(select(ry_metrics, -COMID, -year), by = c("key" = "key"))
rd <- distinct(d, key, .keep_all = TRUE)          # one row per reach-year

stopifnot(max(abs(rd$idx_mean - rd$T)) < 1e-8)    # idx_mean must reproduce T

metrics <- setdiff(names(ry_metrics), c("COMID", "year", "key", "idx_mean"))

h1_cor <- tibble(
  metric = metrics,
  mean   = map_dbl(metrics, \(m) mean(rd[[m]])),
  sd     = map_dbl(metrics, \(m) sd(rd[[m]])),
  r_T    = map_dbl(metrics, \(m) cor(rd[[m]], rd$T)),      # redundancy with the index
  r_Tbar = map_dbl(metrics, \(m) cor(rd[[m]], rd$T_bar))
)
write_csv(rd |> select(COMID, stream, year, key, T, starts_with(c("idx_", "hold_"))),
          file.path(out_tab, "H1_window_metrics.csv"))
write_csv(h1_cor, file.path(out_tab, "H1_metric_correlations.csv"))

# -----------------------------------------------------------------------------
# H2. Add each metric (z-scored) to the final model; compare AIC with the
# final model alone. Two versions:
#   raw   : the metric as is
#   resid : the part of the metric not predicted by T (residual of a
#           reach-year regression on T), i.e. shape information beyond the mean
# Run with all streams and without Big Creek, because elevation and post-spawn
# indices improved fit only through Big Creek (Appendix A6).
# -----------------------------------------------------------------------------
fit_ml <- function(formula, data) lmer(formula, data = data, REML = FALSE, control = ctl)

test_metrics <- function(dat, label) {
  base <- fit_ml(FINAL_FORMULA, dat)
  r <- distinct(dat, key, .keep_all = TRUE)
  map_dfr(metrics, function(m) {
    dat$z  <- as.numeric(scale(dat[[m]]))
    res    <- resid(lm(reformulate("T", m), data = r))
    dat$zr <- as.numeric(scale(res[match(dat$key, r$key)]))
    f_raw   <- suppressMessages(fit_ml(update(FINAL_FORMULA, . ~ . + z),  dat))
    f_resid <- suppressMessages(fit_ml(update(FINAL_FORMULA, . ~ . + zr), dat))
    tibble(
      set = label, metric = m,
      dAIC_raw   = AIC(f_raw) - AIC(base),
      b_raw      = fixef(f_raw)["z"],              # days per SD of the metric
      se_raw     = sqrt(vcov(f_raw)["z", "z"]),
      dAIC_resid = AIC(f_resid) - AIC(base),
      b_resid    = fixef(f_resid)["zr"],
      se_resid   = sqrt(vcov(f_resid)["zr", "zr"]),
      T_bar_with_metric = fixef(f_raw)["T_bar"]     # does the metric take over T_bar?
    )
  })
}

h2 <- bind_rows(
  test_metrics(d, "All streams"),
  test_metrics(droplevels(filter(d, stream != "Big")), "Without Big Creek")
)
write_csv(h2, file.path(out_tab, "H2_metric_added_to_final.csv"))

# -----------------------------------------------------------------------------
# H3. Holding-window cooling rate: between reaches or within reaches?
# hold_slope varies among reaches (a persistent reach property: groundwater,
# shading, elevation, channel form) and among years within a reach. H2 mixes
# the two. Split it exactly as the thermal index is split:
#   C_bar : reach mean of hold_slope over the years the reach held redds
#   C_dev : reach-year departure from that mean
# A spawning CUE should show up in C_dev: in a year when a reach cools more
# slowly than it usually does, fish there spawn later. If only C_bar matters,
# the result is another between-reach contrast, open to the same alternatives
# as T_bar (anything that differs among places).
# Network-wide year-to-year differences are absorbed by the year fixed effect.
# -----------------------------------------------------------------------------
cool <- rd |>
  group_by(COMID) |>
  mutate(C_bar = mean(hold_slope), C_dev = hold_slope - C_bar) |>
  ungroup() |>
  select(key, C_bar, C_dev)
d3 <- left_join(d, cool, by = "key")

# Share of reach-year variation in hold_slope that lies between reaches
share_between <- var(cool$C_bar) / var(rd$hold_slope)   # reach-year level

split_fit <- function(dat, label) {
  base <- fit_ml(FINAL_FORMULA, dat)
  fb <- suppressMessages(fit_ml(update(FINAL_FORMULA, . ~ . + C_bar), dat))
  fd <- suppressMessages(fit_ml(update(FINAL_FORMULA, . ~ . + C_dev), dat))
  fx <- suppressMessages(fit_ml(update(FINAL_FORMULA, . ~ . + C_bar + C_dev), dat))
  co <- function(f, v) c(b = unname(fixef(f)[v]), se = sqrt(vcov(f)[v, v]))
  tibble(
    set = label,
    term = c("C_bar alone", "C_dev alone", "C_bar (both)", "C_dev (both)"),
    dAIC = c(AIC(fb), AIC(fd), AIC(fx), AIC(fx)) - AIC(base),
    b_days_per_C_per_10d = c(co(fb, "C_bar")["b"], co(fd, "C_dev")["b"],
                             co(fx, "C_bar")["b"], co(fx, "C_dev")["b"]),
    se = c(co(fb, "C_bar")["se"], co(fd, "C_dev")["se"],
           co(fx, "C_bar")["se"], co(fx, "C_dev")["se"])
  )
}
h3 <- bind_rows(
  split_fit(d3, "All streams"),
  split_fit(droplevels(filter(d3, stream != "Big")), "Without Big Creek")
) |>
  mutate(share_between_reach = share_between)
write_csv(h3, file.path(out_tab, "H3_cooling_split.csv"))

# Figure: reach-year mean spawn date (adjusted for stream and year) against
# holding-window cooling rate, colored by stream
fit_final <- lmer(FINAL_FORMULA, data = d, control = ctl)
p_h3 <- adjust_stream_year(d, fit_final) |>
  group_by(key, stream) |>
  summarise(y_adj = mean(y_adj), hold_slope = first(hold_slope), n = n(), .groups = "drop") |>
  ggplot(aes(hold_slope, y_adj)) +
  geom_point(aes(color = stream, size = n), alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ x, color = "black", linewidth = 0.6) +
  scale_color_manual(values = cols.streams, name = NULL) +
  scale_size_area(max_size = 4, guide = "none") +
  scale_y_continuous(labels = \(x) format(fixed_date + x, "%d %b")) +
  labs(x = "Holding-window temperature trend, 17 Jul - 15 Aug (°C per 10 d)",
       y = "Mean spawn date\n(adjusted for stream and year)")
ggsave(file.path(out_plot, "H3_cooling_rate.png"), p_h3, width = 6.5, height = 4, dpi = 300)

print(h1_cor, n = Inf); print(h2, n = Inf); print(h3)
cat("Share of hold_slope variation between reaches:", round(share_between, 2), "\n")

# =============================================================================
# How we might use this
#
# - Index-window shape (SD, trend, peak, range, day-to-day change) adds nothing
#   beyond the mean. 
# - Holding-window mean, peak and variability improve fit only through Big
#   Creek, the same pattern as elevation and the post-spawn indices. Treat them
#   as more evidence that Big Creek's gradient is a separate story.
# - Holding-window cooling rate is the one lead: it is nearly unrelated to the
#   index (r = -0.19) and the raw metric helps with and without Big Creek
#   (dAIC -4 to -5; ~3 d later spawning per SD of slower cooling). But it is
#   one of 11 metrics tested and the gain is modest.
# - H3 shows the signal is not stable. About a quarter of the variation in 
#   cooling rate lies between reaches. Entered alone, neither part is 
#   convincing. Entered together, both turn positive and the fit improves. 
#   Estimates that change sign depending on what else is thers is a warning. Do
#   not add it to the current paper.
# =============================================================================
