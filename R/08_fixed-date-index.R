# =============================================================================
# 08_fixed-date-index.R
#
# CJFAS revision: choose a thermal index that does NOT depend on each redd's
# spawn date (see R/07_window-artefact-diagnostics.R for why temp_90 does).
#
# Every candidate is a COMID-year value fixed in calendar time:
#   - window means of 30/60/90 d ending on fixed dates, 1 Jul - 15 Aug
#   - seasonal summaries: mean Jul-Aug, max 7-d mean (MWAT)
# Each is fit with the same random-effect structure as the final model in
# R/09 (COMID and reach-year random intercepts; ML fits for AIC):
#   yday ~ T + stream + year + (1 | COMID) + (1 | COMID:year)        (linear)
#   yday ~ ns(T, 3) + stream + year + (1 | COMID) + (1 | COMID:year)  (spline)
# Candidates are ranked by the linear model (the form used in R/09). The
# reach-year term matters: T is one value per reach-year, so without it redds
# in a reach-year are treated as independent and AIC rewards whichever index
# happens to absorb reach-year noise. AIC_linear_noRY (COMID intercept only)
# is kept to show this. Also reported at the reach-year level (weighted mean
# spawn date ~ T + stream + year).
#
# Outputs: tables/revision/E1_fixed_date_candidates.csv, plots/revision/E1_*.png
# =============================================================================

library(lme4)
library(splines)
source(here::here("R", "revision_setup.R"))   # redds, temps, win_mean(), out_tab, out_plot

doy_lab <- function(d) format(as.Date(d - 1, "2002-01-01"), "%d %b")
first_redd <- min(redds$yday)

# -----------------------------------------------------------------------------
# Candidate indices at the COMID-year level
# -----------------------------------------------------------------------------
cy <- redds |> distinct(key, COMID, year)

grid <- expand_grid(end_doy = seq(182, 227, by = 5), n = c(30, 60, 90)) |>
  mutate(index = sprintf("mean%d_end%s", n, gsub(" ", "", doy_lab(end_doy))))

idx_long <- pmap_dfr(grid, \(end_doy, n, index) {
  cy |> mutate(index = index, end_doy = end_doy, n = n, T = win_mean(key, end_doy, n))
})

# Seasonal summaries
mwat <- temps |>
  filter(doy >= 152, doy <= 273) |>
  arrange(COMID, year, doy) |>
  group_by(COMID, year) |>
  summarise(T = max(stats::filter(temp, rep(1 / 7, 7), sides = 1), na.rm = TRUE), .groups = "drop") |>
  mutate(key = paste(COMID, year, sep = "_"))

seasonal <- bind_rows(
  cy |> mutate(index = "mean_Jul1-Aug31", end_doy = 243, n = 62, T = win_mean(key, 243, 62)),
  cy |> left_join(mwat |> select(key, T), by = "key") |>
    mutate(index = "MWAT_Jun-Sep", end_doy = NA, n = 7)
)
idx_long <- bind_rows(idx_long, seasonal)

# -----------------------------------------------------------------------------
# Fit each candidate
# -----------------------------------------------------------------------------
r2_marg <- function(m) {
  vf <- var(as.vector(model.matrix(m) %*% fixef(m)))
  vr <- sum(sapply(VarCorr(m), \(v) sum(diag(v))))
  vf / (vf + vr + sigma(m)^2)
}

ctl <- lmerControl(optimizer = "bobyqa")

fit_index <- function(d_idx) {
  d <- redds |> left_join(d_idx |> select(key, T), by = "key") |> filter(!is.na(T))
  d$Ts <- (d$T - mean(d$T)) / sd(d$T)
  d$key <- factor(d$key)
  m_lin  <- lmer(yday ~ T + stream + factor(year) + (1 | COMID) + (1 | key), data = d, REML = FALSE, control = ctl)
  m_sp   <- lmer(yday ~ ns(Ts, 3) + stream + factor(year) + (1 | COMID) + (1 | key), data = d, REML = FALSE, control = ctl)
  m_noRY <- lmer(yday ~ T + stream + factor(year) + (1 | COMID), data = d, REML = FALSE, control = ctl)
  rc <- d |> group_by(key, stream, year) |>
    summarise(yday = mean(yday), T = first(T), w = n(), .groups = "drop")
  m_rc  <- lm(yday ~ T + stream + factor(year), data = rc, weights = w)
  m_rc0 <- lm(yday ~ T, data = rc, weights = w)
  tibble(n_redds = nrow(d),
         AIC_linear = AIC(m_lin), AIC_spline = AIC(m_sp), AIC_linear_noRY = AIC(m_noRY),
         slope_lmer = fixef(m_lin)[["T"]],
         slope_lmer_se = sqrt(diag(vcov(m_lin)))[2],
         R2m_linear = r2_marg(m_lin),
         rmse_linear = sqrt(mean(residuals(m_lin)^2)),
         r_reach_year = cor(rc$T, rc$yday),
         slope_reach_year = coef(m_rc)[["T"]],
         R2_reach_year_T_only = summary(m_rc0)$r.squared)
}

E1 <- idx_long |>
  group_by(index, end_doy, n) |>
  group_modify(\(d, k) fit_index(d)) |>
  ungroup() |>
  mutate(end_date = ifelse(is.na(end_doy), NA, doy_lab(end_doy)),
         pre_spawn = !is.na(end_doy) & end_doy < first_redd,
         dAIC_linear = AIC_linear - min(AIC_linear),
         dAIC_spline_vs_linear = AIC_spline - AIC_linear,
         dAIC_linear_noRY = AIC_linear_noRY - min(AIC_linear_noRY)) |>
  arrange(AIC_linear) |>
  relocate(index, end_date, n, pre_spawn, dAIC_linear)

# Reference: the date-anchored temp_90 under the same model (NOT a valid
# comparison of fit -- included only to show how much the artefact inflates)
ref <- redds |> mutate(key = factor(key))
m_ref <- lmer(yday ~ temp_90 + stream + factor(year) + (1 | COMID) + (1 | key), data = ref, REML = FALSE, control = ctl)
message(sprintf("Reference (date-anchored temp_90): AIC %.0f, R2m %.2f, RMSE %.2f d",
                AIC(m_ref), r2_marg(m_ref), sqrt(mean(residuals(m_ref)^2))))

print(E1 |> select(index, pre_spawn, dAIC_linear, dAIC_linear_noRY, dAIC_spline_vs_linear,
                   slope_lmer, slope_lmer_se, R2m_linear, r_reach_year), n = 40)
message(sprintf("AIC spread across candidates: %.1f with reach-year RE vs %.1f without",
                diff(range(E1$AIC_linear)), diff(range(E1$AIC_linear_noRY))))
write_csv(E1, file.path(out_tab, "E1_fixed_date_candidates.csv"))

# -----------------------------------------------------------------------------
# Plot: fit and effect size by window end date and length
# -----------------------------------------------------------------------------
pd <- E1 |> filter(!is.na(end_doy), n %in% c(30, 60, 90)) |>
  mutate(end = as.Date(end_doy - 1, "2002-01-01"), window = factor(paste(n, "d")))
p1 <- ggplot(pd, aes(end, dAIC_linear, colour = window)) +
  geom_line() + geom_point() +
  geom_vline(xintercept = as.Date(first_redd - 1, "2002-01-01"), linetype = 2) +
  labs(x = "Window end date", y = "\u0394AIC (linear model, reach-year RE)", colour = "Window") + theme_bw()
p2 <- ggplot(pd, aes(end, slope_lmer, colour = window)) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin = slope_lmer - 2 * slope_lmer_se, ymax = slope_lmer + 2 * slope_lmer_se), width = 1, show.legend = FALSE) +
  geom_vline(xintercept = as.Date(first_redd - 1, "2002-01-01"), linetype = 2) +
  labs(x = "Window end date", y = "Linear slope (d per \u00B0C)", colour = "Window") + theme_bw()
p3 <- ggplot(pd, aes(end, r_reach_year, colour = window)) +
  geom_line() + geom_point() +
  geom_vline(xintercept = as.Date(first_redd - 1, "2002-01-01"), linetype = 2) +
  labs(x = "Window end date", y = "r (reach-year mean spawn date)", colour = "Window") + theme_bw()
ggsave(file.path(out_plot, "E1_fixed_date_candidates.png"),
       patchwork::wrap_plots(p1, p2, p3, ncol = 1, guides = "collect") +
         patchwork::plot_annotation(subtitle = "Dashed line: first observed redd (any windows ending after it are still fixed in calendar time)"),
       width = 7, height = 9, dpi = 300)
message("Done.")
