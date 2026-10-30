# =============================================================================
# 08_fixed-date-index.R
#
# CJFAS revision: choose a thermal index that does NOT depend on each redd's
# spawn date (see R/07_window-artefact-diagnostics.R for why temp_90 does).
#
# Every candidate is a COMID-year value fixed in calendar time:
#   - window means of 30/60/90 d ending on fixed dates, 1 Jul - 15 Aug
#   - seasonal summaries: mean Jul-Aug, max 7-d mean (MWAT)
# Each is fit with the same model:
#   yday ~ ns(T, 3) + stream + year + (1 | COMID)     (ML, for AIC)
#   yday ~ T + stream + year + (1 | COMID)            (linear: slope, d per C)
# and at the reach-year level (weighted mean spawn date ~ T + stream + year).
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

fit_index <- function(d_idx) {
  d <- redds |> left_join(d_idx |> select(key, T), by = "key") |> filter(!is.na(T))
  d$Ts <- (d$T - mean(d$T)) / sd(d$T)
  m_sp  <- lmer(yday ~ ns(Ts, 3) + stream + factor(year) + (1 | COMID), data = d, REML = FALSE)
  m_lin <- lmer(yday ~ T + stream + factor(year) + (1 | COMID), data = d, REML = FALSE)
  rc <- d |> group_by(key, stream, year) |>
    summarise(yday = mean(yday), T = first(T), w = n(), .groups = "drop")
  m_rc  <- lm(yday ~ T + stream + factor(year), data = rc, weights = w)
  m_rc0 <- lm(yday ~ T, data = rc, weights = w)
  tibble(n_redds = nrow(d),
         AIC_spline = AIC(m_sp), AIC_linear = AIC(m_lin),
         slope_lmer = fixef(m_lin)[["T"]],
         slope_lmer_se = sqrt(diag(vcov(m_lin)))[2],
         R2m_spline = r2_marg(m_sp),
         rmse_spline = sqrt(mean(residuals(m_sp)^2)),
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
         dAIC_spline = AIC_spline - min(AIC_spline)) |>
  arrange(AIC_spline) |>
  relocate(index, end_date, n, pre_spawn, dAIC_spline)

# Reference: the date-anchored temp_90 under the same model (NOT a valid
# comparison of fit -- included only to show how much the artefact inflates)
ref <- redds |> mutate(Ts = (temp_90 - mean(temp_90)) / sd(temp_90))
m_ref <- lmer(yday ~ ns(Ts, 3) + stream + factor(year) + (1 | COMID), data = ref, REML = FALSE)
message(sprintf("Reference (date-anchored temp_90): AIC %.0f, R2m %.2f, RMSE %.2f d",
                AIC(m_ref), r2_marg(m_ref), sqrt(mean(residuals(m_ref)^2))))

print(E1 |> select(index, pre_spawn, dAIC_spline, slope_lmer, R2m_spline, rmse_spline,
                   r_reach_year, slope_reach_year), n = 40)
write_csv(E1, file.path(out_tab, "E1_fixed_date_candidates.csv"))

# -----------------------------------------------------------------------------
# Plot: fit and effect size by window end date and length
# -----------------------------------------------------------------------------
pd <- E1 |> filter(!is.na(end_doy), n %in% c(30, 60, 90)) |>
  mutate(end = as.Date(end_doy - 1, "2002-01-01"), window = factor(paste(n, "d")))
p1 <- ggplot(pd, aes(end, dAIC_spline, colour = window)) +
  geom_line() + geom_point() +
  geom_vline(xintercept = as.Date(first_redd - 1, "2002-01-01"), linetype = 2) +
  labs(x = "Window end date", y = "ΔAIC (spline model)", colour = "Window") + theme_bw()
p2 <- ggplot(pd, aes(end, slope_lmer, colour = window)) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin = slope_lmer - 2 * slope_lmer_se, ymax = slope_lmer + 2 * slope_lmer_se), width = 1) +
  geom_vline(xintercept = as.Date(first_redd - 1, "2002-01-01"), linetype = 2) +
  labs(x = "Window end date", y = "Linear slope (d per °C)", colour = "Window") + theme_bw()
p3 <- ggplot(pd, aes(end, r_reach_year, colour = window)) +
  geom_line() + geom_point() +
  geom_vline(xintercept = as.Date(first_redd - 1, "2002-01-01"), linetype = 2) +
  labs(x = "Window end date", y = "r (reach-year mean spawn date)", colour = "Window") + theme_bw()
ggsave(file.path(out_plot, "E1_fixed_date_candidates.png"),
       patchwork::wrap_plots(p1, p2, p3, ncol = 1, guides = "collect") +
         patchwork::plot_annotation(subtitle = "Dashed line: first observed redd (any windows ending after it are still fixed in calendar time)"),
       width = 7, height = 9, dpi = 300)
message("Done.")
