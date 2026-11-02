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
# E2 (post-spawn / incubation indices; suggested by M. Sparks): tests the
# hypothesis that spawn timing tracks incubation conditions so that fry emerge
# in a favourable spring window (Brannon et al. 2004; Heggberget 1988). Each
# index is a reach-year value on FIXED dates after the spawning season (the
# submission's post-spawn windows started on each redd's own date and had the
# mirror-image artefact). Prediction: colder/slower incubation -> earlier
# spawning, i.e. a positive slope on incubation temperature and a negative
# slope on predicted days to emergence.
#   fall    1 Sep - 30 Nov (spawn year)
#   winter  1 Dec - 28 Feb
#   spring  1 Mar - 30 Apr (following year)
#   incub   1 Sep - 30 Apr
#   emerge  days to emergence for a fixed reference spawn date (1 Sep and
#           15 Aug), Beacham & Murray (1990) Chinook emergence model 2, as
#           implemented in hatchR (model_select(); falls back to the same
#           expression if hatchR is not installed)
# Each is fit (i) alone with stream + year, and (ii) added to the final model
# (T_bar + T_dev + stream + year), all with (1|COMID) + (1|COMID:year);
# (ii) is repeated without Big Creek (see R/09 F5).
#
# Outputs (tables/revision/, plots/revision/):
#   E1_fixed_date_candidates.csv / .png
#   E2_post_spawn_indices.csv, E2_post_spawn_index_values.csv
# =============================================================================

library(lme4)
library(splines)
source(here::here("R", "revision_setup.R"))   # redds, temps, win_mean(), win_mean_dates(), ctl
theme_set(theme_mfsr())

first_redd <- min(redds$yday)                 # windows ending before the first redd are pre-spawn

# =============================================================================
# E1: pre-spawn (and summer) indices
# =============================================================================
# One row per reach-year; every candidate index is computed for each
cy <- redds |> distinct(key, COMID, year)

# Window means: 30, 60 or 90 d ending every 5 days from 1 Jul (DOY 182) to
# 15 Aug (DOY 227)
grid <- expand_grid(end_doy = seq(182, 227, by = 5), n = c(30, 60, 90)) |>
  mutate(index = sprintf("mean%d_end%s", n, gsub(" ", "", doy_lab(end_doy))))

idx_long <- pmap_dfr(grid, \(end_doy, n, index) {
  cy |> mutate(index = index, end_doy = end_doy, n = n, T = win_mean(key, end_doy, n))
})

# Seasonal summaries: mean 1 Jul - 31 Aug, and MWAT (maximum 7-day running
# mean, 1 Jun - 30 Sep; stats::filter with sides = 1 gives the trailing mean)
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
#   m_lin   linear, reach + reach-year intercepts (used for ranking)
#   m_sp    natural spline (df = 3), same random effects
#   m_noRY  linear, reach intercept only (shows why the reach-year term matters)
#   m_rc    reach-year means: weighted mean spawn date ~ T + stream + year
# r2_marg() is a fast marginal R2 (Nakagawa); performance::r2_nakagawa gives
# the same value but is slow over 32 candidates.
# -----------------------------------------------------------------------------
r2_marg <- function(m) {
  vf <- var(as.vector(model.matrix(m) %*% fixef(m)))
  vr <- sum(sapply(VarCorr(m), \(v) sum(diag(v))))
  vf / (vf + vr + sigma(m)^2)
}

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
         slope_lmer_se = sqrt(vcov(m_lin)["T", "T"]),
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
m_ref <- lmer(yday ~ temp_90 + stream + factor(year) + (1 | COMID) + (1 | key),
              data = redds, REML = FALSE, control = ctl)
message(sprintf("Reference (date-anchored temp_90): AIC %.0f, R2m %.2f, RMSE %.2f d",
                AIC(m_ref), r2_marg(m_ref), sqrt(mean(residuals(m_ref)^2))))

print(E1 |> select(index, pre_spawn, dAIC_linear, dAIC_linear_noRY, dAIC_spline_vs_linear,
                   slope_lmer, slope_lmer_se, R2m_linear, r_reach_year), n = 40)
message(sprintf("AIC spread across candidates: %.1f with reach-year RE vs %.1f without",
                diff(range(E1$AIC_linear)), diff(range(E1$AIC_linear_noRY))))
write_csv(E1, file.path(out_tab, "E1_fixed_date_candidates.csv"))

# -----------------------------------------------------------------------------
# Plot: fit (A), effect size +/- 2 SE (B) and reach-year correlation (C) by
# window end date and length. Dashed line: first observed redd.
# -----------------------------------------------------------------------------
pd <- E1 |> filter(!is.na(end_doy), n %in% c(30, 60, 90)) |>
  mutate(end = fixed_date + end_doy, window = factor(paste(n, "d")))
e1_panel <- function(y, ylab) {
  ggplot(pd, aes(end, .data[[y]], colour = window)) +
    geom_line() + geom_point() +
    geom_vline(xintercept = fixed_date + first_redd, linetype = 2) +
    labs(x = "Window end date", y = ylab, colour = "Window")
}
p1 <- e1_panel("dAIC_linear", "\u0394AIC")
p2 <- e1_panel("slope_lmer", "Slope (d per \u00B0C)") +
  geom_errorbar(aes(ymin = slope_lmer - 2 * slope_lmer_se, ymax = slope_lmer + 2 * slope_lmer_se),
                width = 1, show.legend = FALSE)
p3 <- e1_panel("r_reach_year", "r with reach-year\nmean spawn date")
ggsave(file.path(out_plot, "E1_fixed_date_candidates.png"),
       patchwork::wrap_plots(p1, p2, p3, ncol = 1, guides = "collect") +
         patchwork::plot_annotation(tag_levels = "A"),
       width = 7, height = 8, dpi = 300)

# =============================================================================
# E2: post-spawn (incubation) indices
# =============================================================================
emerge_expr <- if (requireNamespace("hatchR", quietly = TRUE)) {
  hatchR::model_select(author = "Beacham and Murray 1990", species = "chinook",
                       model_id = 2, development_type = "emerge")$expression
} else {
  "1 / exp(6.872 - log(x + 0.332))"   # hatchR model_table: Beacham & Murray 1990, chinook, model 2, emerge
}
# Days to emergence: the model gives the fraction of development completed on
# a day at temperature x (ef = 1 / days-to-emergence at constant x). Summing
# ef day by day from the spawn date, emergence is the first day the sum
# reaches 1 (the effective-value approach used by hatchR::predict_phenology).
days_to_emerge <- function(comid, spawn_date) {
  x <- pmax(temp_series(comid, spawn_date, as.Date(spawn_date) + 364), 0)  # clamp sub-zero to 0 (avoids NaN)
  ef <- eval(parse(text = emerge_expr))
  which(cumsum(ef) >= 1)[1]
}

# Post-spawn indices per reach-year (winter ends on the last day of February)
post <- cy |>
  mutate(fall   = win_mean_dates(COMID, paste0(year, "-09-01"), paste0(year, "-11-30")),
         winter = win_mean_dates(COMID, paste0(year, "-12-01"), paste0(year + 1, "-02-28")),
         spring = win_mean_dates(COMID, paste0(year + 1, "-03-01"), paste0(year + 1, "-04-30")),
         incub  = win_mean_dates(COMID, paste0(year, "-09-01"), paste0(year + 1, "-04-30")),
         emerge_1Sep  = map2_dbl(COMID, paste0(year, "-09-01"), days_to_emerge),
         emerge_15Aug = map2_dbl(COMID, paste0(year, "-08-15"), days_to_emerge))
write_csv(post, file.path(out_tab, "E2_post_spawn_index_values.csv"))

# Add the post-spawn indices to the final-model data and fit reference models
dm <- model_data() |>
  left_join(post |> select(-COMID, -year), by = "key") |>
  mutate(key = factor(key))
dnb <- droplevels(filter(dm, stream != "Big"))      # Big Creek sensitivity (see R/09 F5)
fit_ml <- function(f, data = dm) lmer(f, data, REML = FALSE, control = ctl)
base_full <- fit_ml(yday ~ T_bar + T_dev + stream + year + (1 | COMID) + (1 | key))
base_T    <- fit_ml(yday ~ T + stream + year + (1 | COMID) + (1 | key))
base_none <- fit_ml(yday ~ stream + year + (1 | COMID) + (1 | key))
base_nb   <- fit_ml(yday ~ T_bar + T_dev + stream + year + (1 | COMID) + (1 | key), dnb)

lab_post <- c(fall = "Fall mean, 1 Sep\u201330 Nov", winter = "Winter mean, 1 Dec\u201328 Feb",
              spring = "Spring mean, 1 Mar\u201330 Apr", incub = "Incubation mean, 1 Sep\u201330 Apr",
              emerge_1Sep = "Days to emergence if spawned 1 Sep", emerge_15Aug = "Days to emergence if spawned 15 Aug")
# For each index P: (i) P alone instead of the pre-spawn index; (ii) P added
# to the final model; (iii) as (ii) without Big Creek
E2 <- map_dfr(names(lab_post), \(v) {
  dm$P <- dm[[v]]; dnb$P <- dnb[[v]]      # local copies with P = this index
  m_alone <- fit_ml(yday ~ P + stream + year + (1 | COMID) + (1 | key), dm)
  m_add   <- fit_ml(yday ~ T_bar + T_dev + P + stream + year + (1 | COMID) + (1 | key), dm)
  m_nb    <- fit_ml(yday ~ T_bar + T_dev + P + stream + year + (1 | COMID) + (1 | key), dnb)
  ry <- dm |> distinct(key, .keep_all = TRUE)        # one row per reach-year
  tibble(index = v, label = lab_post[[v]],
         mean_value = mean(ry$P), sd_value = sd(ry$P),
         r_with_prespawn_T = cor(ry$P, ry$T),
         slope_alone = fixef(m_alone)[["P"]], se_alone = sqrt(vcov(m_alone)["P", "P"]),
         dAIC_alone_vs_no_temp = AIC(m_alone) - AIC(base_none),
         dAIC_alone_vs_prespawn_T = AIC(m_alone) - AIC(base_T),
         slope_added = fixef(m_add)[["P"]], se_added = sqrt(vcov(m_add)["P", "P"]),
         dAIC_added_vs_final = AIC(m_add) - AIC(base_full),
         slope_added_noBig = fixef(m_nb)[["P"]], se_added_noBig = sqrt(vcov(m_nb)["P", "P"]),
         dAIC_added_vs_final_noBig = AIC(m_nb) - AIC(base_nb))
})
print(E2 |> select(index, r_with_prespawn_T, slope_alone, se_alone, dAIC_alone_vs_prespawn_T,
                   slope_added, se_added, dAIC_added_vs_final, slope_added_noBig, dAIC_added_vs_final_noBig), width = Inf)
write_csv(E2, file.path(out_tab, "E2_post_spawn_indices.csv"))
message("Emergence model expression: ", emerge_expr)
message("Done.")
