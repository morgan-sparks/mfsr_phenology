# =============================================================================
# 07_window-artifact-diagnostics.R
#
# CJFAS revision (R1 / AE: "temporal correlation" concern; R2 general point 3).
#
# temp_90 is the mean of the 90 days ENDING ON EACH REDD'S OWN SPAWN DATE
# (R/04_comid-temperature.R, period = "before"). Spawn date therefore enters
# both sides of the model: any seasonal trend in stream temperature produces
# a spawn date ~ temp_90 association even if fish do not respond to
# temperature at all. This script quantifies that artifact.
#
# Diagnostics
#   D1. Network-mean seasonal thermograph (where spawning falls relative to
#       the thermal peak).
#   D2. Within reach-year (same COMID, same year; only date differs): how
#       much do 30/60/90-d antecedent means change per day of later spawning?
#   D3. Null model: shuffle spawn dates among redds within year, recompute
#       the antecedent windows at each redd's own COMID, refit. Any
#       association here is produced by window construction alone.
#   D4. Per-COMID within-reach slope (spawn date on temp_90) vs. the slope
#       expected from the artifact alone (1 / rate the window warms).
#
# Outputs (tables/revision/, plots/revision/):
#   D1_seasonal_thermograph.png
#   D2_within_reach_window_change.csv
#   D3_shuffled_date_null.csv (+ _runs.csv), D3_shuffled_date_null.png
#   D4_comid_slope_vs_artifact.csv / .png
# Data and window helper: R/revision_setup.R
# =============================================================================

library(lme4)
library(splines)
source(here::here("R", "revision_setup.R"))   # redds (with temp_30/60/90), temps, win_mean()
theme_set(theme_mfsr())

set.seed(20261030)
n_perm <- 200

# Check: our temp_90 reproduces the values used in the submitted analysis
load(here("data", "processed", "comid_temps.RData"))   # object: out
chk <- out |>
  filter(period == "before", duration == 90) |>
  transmute(redd_id = redd_id, temp_90_saved = avg_temp) |>
  inner_join(redds |> select(redd_id, temp_90), by = "redd_id")
message(sprintf("temp_90 reproduction: n = %d, max |diff| = %.2e",
                nrow(chk), max(abs(chk$temp_90 - chk$temp_90_saved))))
message(sprintf("Redds: %d; COMIDs: %d; COMID-years: %d",
                nrow(redds), n_distinct(redds$COMID), n_distinct(redds$key)))

# -----------------------------------------------------------------------------
# D1. Seasonal thermograph
# -----------------------------------------------------------------------------
# Network mean (and 10th-90th percentile) of daily temperature across all
# surveyed reach-years, April-October
thermo <- temps |>
  group_by(doy) |>
  summarise(mean = mean(temp, na.rm = TRUE),
            lo = quantile(temp, 0.1, na.rm = TRUE),
            hi = quantile(temp, 0.9, na.rm = TRUE), .groups = "drop") |>
  filter(doy >= 91, doy <= 305)
peak_doy <- thermo$doy[which.max(thermo$mean)]
message(sprintf("Network thermal peak: DOY %d (%s), %.1f C; spawning DOY %d-%d",
                peak_doy, doy_lab(peak_doy),
                max(thermo$mean), min(redds$yday), max(redds$yday)))

# Grey band: 10th-90th percentile; blue: observed spawn dates; dashed: peak
p_thermo <- ggplot(thermo, aes(fixed_date + doy)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey85") +
  geom_line(aes(y = mean), linewidth = 0.8) +
  annotate("rect", xmin = fixed_date + min(redds$yday), xmax = fixed_date + max(redds$yday),
           ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "steelblue") +
  geom_vline(xintercept = fixed_date + peak_doy, linetype = 2) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = NULL, y = "Stream temperature (\u00B0C)")
ggsave(file.path(out_plot, "D1_seasonal_thermograph.png"), p_thermo, width = 7, height = 4, dpi = 300)

# -----------------------------------------------------------------------------
# D2. Within reach-year change in antecedent temperature per day of spawn date
# Center spawn date (x) and the window mean (y) within each reach-year, so only
# differences among redds in the same reach and year remain, then regress y
# on x. The slope is how much the window mean changes when the same reach is
# simply sampled later. var_share_within = share of the window mean's total
# variance that lies within reach-years (i.e. driven only by spawn date).
# -----------------------------------------------------------------------------
within_cy <- function(d, var) {
  d <- d |>
    group_by(key) |>
    mutate(x = yday - mean(yday), y = .data[[var]] - mean(.data[[var]])) |>
    ungroup()
  b <- coef(lm(y ~ 0 + x, data = d))[[1]]
  tibble(
    window = var, 
    change_per_10d = 10 * b,
    r_within = cor(d$x, d$y), 
    r_raw = cor(redds$yday, redds[[var]])
  )
}
share_within <- function(d, var) {
  z <- d[[var]] - ave(d[[var]], d$key)
  var(z) / var(d[[var]])
}
D2 <- map_dfr(c("temp_30", "temp_60", "temp_90"), \(v) within_cy(redds, v))
D2$var_share_within <- map_dbl(D2$window, \(v) share_within(redds, v))
print(D2)
write_csv(D2, file.path(out_tab, "D2_within_reach_window_change.csv"))

# -----------------------------------------------------------------------------
# D3. Shuffled-date null
# Shuffle spawn dates among redds within each year (breaking any real link
# between a reach and its timing), recompute temp_90 at each redd's own reach
# for its new date, and refit. Statistics from the shuffled data show what
# window construction alone produces. Stats:
#   r2_quad                = yday ~ temp_90 + temp_90^2 (submitted figure)
#   r2_quad_stream_year    = same + stream + year
#   within_slope_d_per_C   = slope of yday on temp_90 within reach-years
#   lmer_resid_sd          = residual SD of a submitted-style mixed model
# -----------------------------------------------------------------------------
fit_stats <- function(d) {
  d <- d |> mutate(t = temp_90)
  m_q <- lm(yday ~ t + I(t^2), data = d)
  m_sy <- lm(yday ~ t + I(t^2) + stream + factor(year), data = d)
  m_mm <- lmer(yday ~ ns(t, 3) + stream + factor(year) + (1 | COMID),
    data = d, REML = FALSE
  )
  dw <- d |>
    group_by(key) |>
    mutate(x = t - mean(t), y = yday - mean(yday)) |>
    ungroup()
  tibble(
    r2_quad = summary(m_q)$r.squared,
    r2_quad_stream_year = summary(m_sy)$r.squared,
    within_slope_d_per_C = coef(lm(y ~ 0 + x, data = dw))[[1]],
    lmer_resid_sd = sigma(m_mm)
  )
}

obs <- fit_stats(redds) |> mutate(run = "observed")
null <- map_dfr(seq_len(n_perm), \(i) {
  d <- redds |>
    group_by(year) |>
    mutate(yday = sample(yday)) |>
    ungroup() |>
    mutate(temp_90 = win_mean(key, yday, 90))
  fit_stats(d) |> mutate(run = paste0("null_", i))
})

D3 <- bind_rows(
  obs,
  null |> summarise(across(-run, mean)) |> mutate(run = "null_mean"),
  null |> summarise(across(-run, \(x) quantile(x, 0.025))) |> mutate(run = "null_q025"),
  null |> summarise(across(-run, \(x) quantile(x, 0.975))) |> mutate(run = "null_q975")
  ) |> 
  relocate(run)

print(D3)
write_csv(D3, file.path(out_tab, "D3_shuffled_date_null.csv"))
write_csv(null, file.path(out_tab, "D3_shuffled_date_null_runs.csv"))

p_null <- null |>
  select(r2_quad, within_slope_d_per_C) |>
  pivot_longer(everything()) |>
  ggplot(aes(value)) +
  geom_histogram(bins = 30, fill = "grey70", color = "grey40", linewidth = 0.2) +
  geom_vline(
    data = obs |> select(r2_quad, within_slope_d_per_C) |> pivot_longer(everything()),
    aes(xintercept = value), colour = "firebrick", linewidth = 1
  ) +
  facet_wrap(~name,
    scales = "free",
    labeller = as_labeller(c(
      r2_quad = "R\u00B2 (yday ~ temp_90 + temp_90\u00B2)",
      within_slope_d_per_C = "Within reach-year slope (d per \u00B0C)"
    ))
  ) +
  labs(x = NULL, y = "Permutations")
ggsave(file.path(out_plot, "D3_shuffled_date_null.png"), p_null, width = 8, height = 3.5, dpi = 300)

# -----------------------------------------------------------------------------
# D4. Per-COMID within-reach slope vs. artifact-only expectation
# Slope of temperature on date gives the window warming rate b; compares the observed slope with 1/b
# If temp_90 = a + b * yday within a reach-year (b = how fast the 90-d window
# warms as its end date moves later), then regressing yday on temp_90 within
# that reach gives slope 1/b whatever the fish does. For each reach with >= 15
# redds (and some spread in dates), compare the observed slope with 1/b.
# Both slopes use deviations from reach-year means (x, t), so they are
# within-reach-year quantities.
# -----------------------------------------------------------------------------
D4 <- redds |>
  group_by(key) |>
  mutate(x = yday - mean(yday), t = temp_90 - mean(temp_90)) |>
  group_by(COMID, stream) |>
  filter(n() >= 15, var(x) >= 4) |>
  summarise(
    n = n(),
    window_warming_C_per_d = coef(lm(t ~ 0 + x))[[1]],
    slope_observed = coef(lm(x ~ 0 + t))[[1]],
    .groups = "drop"
  ) |>
  mutate(slope_artifact = 1 / window_warming_C_per_d)
message(sprintf(
  "D4: %d COMIDs; r(observed slope, artifact-only slope) = %.2f; medians %.1f vs %.1f d/C",
  nrow(D4), cor(D4$slope_observed, D4$slope_artifact),
  median(D4$slope_observed), median(D4$slope_artifact)
))
write_csv(D4, file.path(out_tab, "D4_comid_slope_vs_artifact.csv"))

p_d4 <- ggplot(D4, aes(slope_artifact, slope_observed, colour = stream)) +
  geom_abline(linetype = 2) +
  geom_point(aes(size = n)) +
  scale_colour_manual(values = cols.streams) +
  labs(
    x = "Artifact-only slope: 1 / window warming rate (d per \u00B0C)",
    y = "Observed within-reach slope (d per \u00B0C)", colour = "Stream", size = "Redds"
  )
ggsave(file.path(out_plot, "D4_comid_slope_vs_artifact.png"), p_d4, width = 6.5, height = 4.5, dpi = 300)
