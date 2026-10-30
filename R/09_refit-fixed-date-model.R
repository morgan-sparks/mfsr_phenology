# =============================================================================
# 09_refit-fixed-date-model.R
#
# CJFAS revision: refit the spawn-timing model with a thermal index fixed in
# calendar time (chosen from R/08_fixed-date-index.R), replacing the
# date-anchored temp_90.
#
#   F1  model comparison (ML): no temperature, linear, spline, quadratic,
#       within-between, random slopes, and no reach-year term; plus the
#       submitted temp_90 model for reference only
#   F2  final model (REML) coefficients
#
# Random effects: the thermal index is a reach-year value, so redds in the
# same COMID-year share it. All candidate models therefore include a
# reach-year random intercept (1 | key), key = COMID_year, in addition to
# (1 | COMID). Dropping it inflates precision (pseudoreplication) and lets
# random slopes absorb reach-year noise (see F1).
#   F3  estimated marginal means + Tukey contrasts for stream and year
#   F4  COMID random intercepts vs network position (R2 general points 1-2)
#   plots: population-level temperature response; COMID intercepts vs
#          drainage area / elevation within stream
#
# Change the index here:
# =============================================================================
INDEX_END_DOY <- 197   # 16 Jul (DOY in 2002; non-leap years 2002-2005)
INDEX_N       <- 60    # window length (days): 18 May - 16 Jul

library(lme4)
library(splines)
library(emmeans)
library(performance)
source(here::here("R", "revision_setup.R"))   # redds, temps, win_mean(), out_tab, out_plot
emm_options(lmer.df = "asymptotic")            # n > 3000: Satterthwaite/KR disabled anyway

doy_lab <- function(d) format(as.Date(d - 1, "2002-01-01"), "%d %b")
index_label <- sprintf("%d-d mean ending %s (%s - %s)", INDEX_N, doy_lab(INDEX_END_DOY),
                       doy_lab(INDEX_END_DOY - INDEX_N + 1), doy_lab(INDEX_END_DOY))
message("Thermal index: ", index_label)

# -----------------------------------------------------------------------------
# Data
# -----------------------------------------------------------------------------
d <- redds |>
  mutate(T = win_mean(key, INDEX_END_DOY, INDEX_N)) |>
  filter(!is.na(T)) |>
  group_by(COMID) |>
  mutate(T_bar = mean(T[!duplicated(key)])) |>    # reach mean over years sampled
  ungroup() |>
  mutate(T_dev = T - T_bar,                         # reach-year anomaly
         year  = factor(year),
         COMID = factor(COMID),
         key   = factor(key))
mu_T <- mean(d$T); sd_T <- sd(d$T)
d <- d |> mutate(Ts = (T - mu_T) / sd_T,
                 t90s = (temp_90 - mean(temp_90)) / sd(temp_90))
message(sprintf("n = %d redds, %d COMIDs, %d COMID-years; index range %.1f-%.1f C",
                nrow(d), n_distinct(d$COMID), n_distinct(d$key), min(d$T), max(d$T)))

# -----------------------------------------------------------------------------
# F1 Model comparison (ML)
# -----------------------------------------------------------------------------
ctl <- lmerControl(optimizer = "bobyqa")
mods <- list(
  "no temperature"            = lmer(yday ~ stream + year + (1 | COMID) + (1 | key), d, REML = FALSE, control = ctl),
  "T linear"                  = lmer(yday ~ T + stream + year + (1 | COMID) + (1 | key), d, REML = FALSE, control = ctl),
  "T spline (df = 3)"         = lmer(yday ~ ns(Ts, 3) + stream + year + (1 | COMID) + (1 | key), d, REML = FALSE, control = ctl),
  "T quadratic"               = lmer(yday ~ Ts + I(Ts^2) + stream + year + (1 | COMID) + (1 | key), d, REML = FALSE, control = ctl),
  "within-between (T_bar + T_dev)" = lmer(yday ~ T_bar + T_dev + stream + year + (1 | COMID) + (1 | key), d, REML = FALSE, control = ctl),
  "T linear + random slopes"  = lmer(yday ~ Ts + stream + year + (1 + Ts | COMID) + (1 | key), d, REML = FALSE, control = ctl),
  "T linear, no reach-year RE" = lmer(yday ~ T + stream + year + (1 | COMID), d, REML = FALSE, control = ctl),
  "T linear + random slopes, no reach-year RE" = lmer(yday ~ Ts + stream + year + (1 + Ts | COMID), d, REML = FALSE, control = ctl)
)
ref <- lmer(yday ~ ns(t90s, 3) + stream + year + (1 + t90s | COMID), d, REML = FALSE,
            control = lmerControl(optimizer = "bobyqa"))

summ <- function(m, name) {
  r2 <- suppressWarnings(performance::r2_nakagawa(m))
  tibble(model = name, df = attr(logLik(m), "df"), AIC = AIC(m),
         R2_marginal = r2$R2_marginal, R2_conditional = r2$R2_conditional,
         RMSE_days = sqrt(mean(residuals(m)^2)),
         singular = isSingular(m))
}
F1 <- imap_dfr(mods, summ) |>
  mutate(dAIC = AIC - min(AIC)) |>
  arrange(AIC) |>
  bind_rows(summ(ref, "REFERENCE ONLY: submitted model, date-anchored temp_90 (circular)")) |>
  relocate(model, dAIC)
print(F1, width = Inf)
write_csv(F1, file.path(out_tab, "F1_model_comparison_fixed_index.csv"))

wb <- mods[["within-between (T_bar + T_dev)"]]
message(sprintf("Within-between: T_bar %.2f (SE %.2f) d/C; T_dev %.2f (SE %.2f) d/C",
                fixef(wb)["T_bar"], sqrt(vcov(wb)["T_bar", "T_bar"]),
                fixef(wb)["T_dev"], sqrt(vcov(wb)["T_dev", "T_dev"])))
lin <- mods[["T linear"]]
message(sprintf("Linear: %.2f d per C (95%% Wald CI %.2f-%.2f)",
                fixef(lin)["T"], fixef(lin)["T"] - 1.96 * sqrt(vcov(lin)["T", "T"]),
                fixef(lin)["T"] + 1.96 * sqrt(vcov(lin)["T", "T"])))

# -----------------------------------------------------------------------------
# F2 Final model (REML): linear T, stream, year; random intercepts for COMID
#    and reach-year. (Spline/quadratic add nothing once the reach-year term
#    is present; random slopes are singular -- see F1.)
# -----------------------------------------------------------------------------
final <- lmer(yday ~ T + stream + year + (1 | COMID) + (1 | key), d, REML = TRUE, control = ctl)
ci <- confint(final, method = "Wald")
F2 <- tibble(term = names(fixef(final)), estimate = fixef(final),
             se = sqrt(diag(vcov(final)))) |>
  mutate(ci_low = ci[term, 1], ci_high = ci[term, 2],
         p_value = 2 * pnorm(-abs(estimate / se)))
vc <- as.data.frame(VarCorr(final))
r2f <- performance::r2_nakagawa(final)
F2_re <- tibble(term = c("sigma2 (residual)", "tau00 COMID (intercept var)",
                         "tau00 reach-year (intercept var)", "ICC (adjusted)",
                         "R2 marginal", "R2 conditional", "RMSE (days)", "N redds", "N COMID",
                         "N reach-years"),
                estimate = c(vc$vcov[vc$grp == "Residual"], vc$vcov[vc$grp == "COMID"],
                             vc$vcov[vc$grp == "key"],
                             performance::icc(final)$ICC_adjusted,
                             r2f$R2_marginal, r2f$R2_conditional,
                             sqrt(mean(residuals(final)^2)), nrow(d), n_distinct(d$COMID),
                             n_distinct(d$key)))
print(F2, n = 30); print(F2_re)
write_csv(bind_rows(F2, F2_re), file.path(out_tab, "F2_final_model_fixed_index.csv"))

# -----------------------------------------------------------------------------
# F3 Marginal means (at mean T) and Tukey contrasts
# -----------------------------------------------------------------------------
emm_s <- emmeans(final, ~ stream)
emm_y <- emmeans(final, ~ year)
to_date <- function(x) format(as.Date(round(x) - 1, "2002-01-01"), "%d %b")
F3_means <- bind_rows(
  as_tibble(emm_s) |> rename(level = stream) |> mutate(factor = "stream"),
  as_tibble(emm_y) |> rename(level = year)   |> mutate(factor = "year")) |>
  mutate(level = as.character(level), date = to_date(emmean)) |> relocate(factor)
F3_contr <- bind_rows(
  as_tibble(pairs(emm_s, adjust = "tukey")) |> mutate(factor = "stream"),
  as_tibble(pairs(emm_y, adjust = "tukey")) |> mutate(factor = "year")) |> relocate(factor)
print(F3_means)
write_csv(F3_means, file.path(out_tab, "F3_emmeans_fixed_index.csv"))
write_csv(F3_contr, file.path(out_tab, "F3_contrasts_fixed_index.csv"))

# -----------------------------------------------------------------------------
# F4 COMID random intercepts vs network position (Copeland general points 1-2)
#     Position = NHDPlus total drainage area (larger = further downstream).
# -----------------------------------------------------------------------------
con <- DBI::dbConnect(RSQLite::SQLite(), here("data", "processed", "gis", "flowline_pn17_nsi_MFSR.gpkg"))
nhd <- DBI::dbGetQuery(con, "select COMID, LENGTHKM, TotDASqKM from flowline_pn17_nsi_MFSR") |>
  mutate(COMID = as.character(COMID)) |>
  group_by(COMID) |> summarise(length_km = first(LENGTHKM), da_km2 = max(TotDASqKM), .groups = "drop")
DBI::dbDisconnect(con)
elev <- readRDS(here("data", "processed", "elevslope.rds")) |>
  transmute(COMID = as.character(COMID), elev_m = (MAXELEVSMO + MINELEVSMO) / 2 / 100)

re <- ranef(final)$COMID |>
  rownames_to_column("COMID") |>
  rename(intercept = `(Intercept)`) |>
  left_join(d |> distinct(COMID = as.character(COMID), stream), by = "COMID") |>
  left_join(nhd, by = "COMID") |>
  left_join(elev |> distinct(COMID, .keep_all = TRUE), by = "COMID") |>
  left_join(d |> count(COMID = as.character(COMID), name = "n_redds"), by = "COMID")

message(sprintf("COMID length: median %.2f km (range %.2f-%.2f), n = %d",
                median(re$length_km, na.rm = TRUE), min(re$length_km, na.rm = TRUE),
                max(re$length_km, na.rm = TRUE), sum(!is.na(re$length_km))))

F4 <- re |>
  group_by(stream) |>
  filter(n() >= 5) |>
  summarise(n_comid = n(),
            r_intercept_logDA = cor(intercept, log(da_km2), use = "complete.obs"),
            r_intercept_elev  = cor(intercept, elev_m, use = "complete.obs"),
            .groups = "drop")
m_pos <- lmer(intercept ~ log(da_km2) + (1 | stream), data = re)
F4 <- bind_rows(F4, tibble(stream = "ALL (within-stream slope, lmer)",
                           n_comid = nrow(re),
                           r_intercept_logDA = fixef(m_pos)[2],
                           r_intercept_elev = NA_real_))
message(sprintf("Pooled within-stream slope of COMID intercept on log(drainage area): %.2f d (SE %.2f)",
                fixef(m_pos)[2], sqrt(vcov(m_pos)[2, 2])))
print(F4)
write_csv(F4, file.path(out_tab, "F4_comid_intercepts_vs_position.csv"))
write_csv(re, file.path(out_tab, "F4_comid_random_intercepts.csv"))

# -----------------------------------------------------------------------------
# Plots
# -----------------------------------------------------------------------------
cols <- c("Bear Valley" = "#55FF00", "Beaver" = "#7A8EF5", "Big" = "#C9C900", "Camas" = "#E64C00",
          "Elk" = "#B5B57A", "Loon" = "#3FC79A", "Marsh" = "#DF73FF", "Sulphur" = "#FFAA00")

# Population-level response averaged over stream and year (emmeans)
grid_T <- seq(min(d$T), max(d$T), length.out = 60)
pr <- as_tibble(emmeans(final, ~ T, at = list(T = grid_T)))
ry <- d |> group_by(key, stream, year) |>
  summarise(T = first(T), yday = mean(yday), n = n(), .groups = "drop")
p_resp <- ggplot() +
  geom_point(data = ry, aes(T, as.Date(yday - 1, "2002-01-01"), colour = stream, size = n), alpha = 0.7) +
  geom_ribbon(data = pr, aes(T, ymin = as.Date(asymp.LCL - 1, "2002-01-01"),
                             ymax = as.Date(asymp.UCL - 1, "2002-01-01")), alpha = 0.25) +
  geom_line(data = pr, aes(T, as.Date(emmean - 1, "2002-01-01")), linewidth = 1) +
  scale_colour_manual(values = cols) + scale_size_area(max_size = 5) +
  labs(x = paste0("Reach-year thermal index (°C): ", index_label),
       y = "Spawn date", colour = "Stream", size = "Redds",
       subtitle = "Points: reach-year mean spawn date (size = redds). Line: population-level fit, 95% CI") +
  theme_bw()
ggsave(file.path(out_plot, "F_temperature_response_fixed_index.png"), p_resp, width = 8, height = 5, dpi = 300)

p_pos <- ggplot(re, aes(da_km2, intercept, colour = stream)) +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_point(aes(size = n_redds)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.6) +
  scale_x_log10() + scale_colour_manual(values = cols) +
  facet_wrap(~stream, scales = "free_x") +
  labs(x = "Total drainage area (km², log scale; larger = further downstream)",
       y = "COMID random intercept (d)", size = "Redds") +
  theme_bw() + theme(legend.position = "none")
ggsave(file.path(out_plot, "F_comid_intercepts_vs_drainage_area.png"), p_pos, width = 8, height = 6, dpi = 300)

message("Done.")
