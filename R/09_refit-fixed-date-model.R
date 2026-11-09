# =============================================================================
# 09_refit-fixed-date-model.R
#
# CJFAS revision: refit the spawn-timing model with a thermal index fixed in
# calendar time (chosen in R/08), replacing the date-anchored temp_90.
#
# Final model (FINAL_FORMULA in R/revision_setup.R, chosen 2 Nov 2026):
#   yday ~ T_bar + T_dev + stream + year + (1 | COMID) + (1 | COMID:year)
#   T_bar = reach mean of the thermal index (between-reach thermal regime)
#   T_dev = reach-year anomaly from that mean (within-reach, between-year)
# Splitting T this way (within-subject centering; van de Pol & Wright 2009)
# separates differences among places from responses of the same place to
# warmer/cooler years; fixed reach traits cannot confound T_dev.
#
# Random effects: the thermal index is a reach-year value, so redds in the
# same COMID-year share it. All candidate models include a reach-year random
# intercept (1 | key), key = COMID_year, in addition to (1 | COMID).
#
#   F1  model comparison (ML); submitted temp_90 model shown for reference only
#   F2  final model (REML) coefficients and variance components
#   F3  estimated marginal means + Tukey contrasts for stream and year
#   F4  reach random intercepts vs network position (R2 general points 1-2)
#   F5  elevation sensitivity: all streams, without Big Creek, Big vs others
# Manuscript Figure 4 (partial effects) is built in R/10; 
# Table 1 (the polished version of F1) and Table 2 in R/11.
# The thermal index (INDEX_END_DOY, INDEX_N) is set in R/revision_setup.R.
#
# Outputs (tables/revision/, plots/revision/):
#   F1_model_comparison_fixed_index.csv, F2_final_model_fixed_index.csv,
#   F3_emmeans_fixed_index.csv, F3_contrasts_fixed_index.csv,
#   F4_comid_random_intercepts.csv, F4_comid_intercepts_vs_position.csv,
#   F4_comid_intercepts_vs_drainage_area.png,
#   F5_elevation_sensitivity.csv, F5_elevation_span_by_stream.csv
# =============================================================================

library(lme4)
library(splines)
library(emmeans)
source(here::here("R", "revision_setup.R"))  # model_data(), FINAL_FORMULA, ctl, fit_summary()
theme_set(theme_mfsr())
emm_options(lmer.df = "asymptotic")  # z-based CIs; n > 3000 redds, so df-corrections are moot
message("Thermal index: ", index_label)

# -----------------------------------------------------------------------------
# Data
# -----------------------------------------------------------------------------
d <- model_data()
mu_T <- mean(d$T)
sd_T <- sd(d$T)
message(
  sprintf(
    "n = %d redds, %d COMIDs, %d COMID-years; index range %.1f-%.1f C",
    nrow(d), n_distinct(d$COMID), n_distinct(d$key), min(d$T), max(d$T)
  )
)

# -----------------------------------------------------------------------------
# F1 Model comparison (ML). Every model has stream + year and the reach and
# reach-year intercepts unless the name says otherwise; fm() fills these in.
# -----------------------------------------------------------------------------
RE <- "+ stream + year + (1 | COMID) + (1 | key)"
fm <- function(fixed, re = RE, data = d) {
  lmer(as.formula(paste("yday ~", fixed, re)), droplevels(data), REML = FALSE, control = ctl)
}
mods <- list(
  "FINAL: T_bar + T_dev" = fm("T_bar + T_dev"),
  "T (not split)" = fm("T"),
  "T_bar only" = fm("T_bar"),
  "T_dev only" = fm("T_dev"),
  "no temperature" = fm("1"),
  "FINAL + elevation (see F5)" = fm("T_bar + T_dev + E"),
  "FINAL + reach slope" = fm("T_bar + T_dev + slope_s"),
  "FINAL with spline on T_bar (df = 3)" = fm("ns(T_bar, 3) + T_dev"),
  "FINAL + random slopes for T_dev" = fm("T_bar + T_dev", "+ stream + year + (1 + T_dev | COMID) + (1 | key)"),
  "FINAL without reach-year RE" = fm("T_bar + T_dev", "+ stream + year + (1 | COMID)")
)
# The submitted model (spline on date-anchored temp_90, random slopes), for
# reference only: its fit is inflated by the artifact (R/07)
ref <- lmer(yday ~ ns(t90s, 3) + stream + year + (1 + t90s | COMID), d, REML = FALSE, control = ctl)

# Map fit_summary() to the mods
F1 <- imap_dfr(mods, \(m, name) fit_summary(m, name)) |>
  mutate(dAIC = AIC - AIC[1]) |>
  arrange(AIC) |>
  bind_rows(fit_summary(ref, "REFERENCE ONLY: submitted model, date-anchored temp_90 (circular)")) |>
  relocate(model, dAIC)
print(F1, width = Inf)
write_csv(F1, file.path(out_tab, "F1_model_comparison_fixed_index.csv"))

pr_coef <- function(m, terms) {
  paste(sprintf(
    "%s %.2f (SE %.2f)", terms, fixef(m)[terms],
    sqrt(diag(vcov(m))[terms])
  ), collapse = "; ")
}
message("T (not split): ", pr_coef(mods[["T (not split)"]], "T"))
message("FINAL: ", pr_coef(mods[[1]], c("T_bar", "T_dev")))
message("FINAL + elevation: ", pr_coef(mods[["FINAL + elevation (see F5)"]], c("T_bar", "T_dev", "E")))

# -----------------------------------------------------------------------------
# F2 Final model (REML)
# -----------------------------------------------------------------------------
final <- lmer(FINAL_FORMULA, d, REML = TRUE, control = ctl)
ci <- confint(final, method = "Wald", parm = "beta_")
F2 <- tibble(
  term = names(fixef(final)), 
  estimate = fixef(final),
  se = sqrt(diag(vcov(final)))
  ) |>
  mutate(
    ci_low = ci[term, 1], 
    ci_high = ci[term, 2],
    p_value = 2 * pnorm(-abs(estimate / se))
  )
vc <- as.data.frame(VarCorr(final))
r2f <- performance::r2_nakagawa(final)
F2_re <- tibble(
  term = c(
    "sigma2 (residual)", "tau00 COMID (intercept var)",
    "tau00 reach-year (intercept var)", 
    "ICC (adjusted)",
    "R2 marginal", "R2 conditional", 
    "RMSE (days)", "N redds", "N COMID",
    "N reach-years"
  ),
  estimate = c(
    vc$vcov[vc$grp == "Residual"], vc$vcov[vc$grp == "COMID"],
    vc$vcov[vc$grp == "key"],
    performance::icc(final)$ICC_adjusted,
    r2f$R2_marginal, r2f$R2_conditional,
    sqrt(mean(residuals(final)^2)), nrow(d), n_distinct(d$COMID),
    n_distinct(d$key)
  )
)
print(F2, n = 30)
print(F2_re)
write_csv(bind_rows(F2, F2_re), file.path(out_tab, "F2_final_model_fixed_index.csv"))

# -----------------------------------------------------------------------------
# F3 Marginal means (at mean T_bar and mean T_dev ~ 0) and Tukey contrasts
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
# F4 Reach random intercepts vs network position (R2 general points 1-2)
#    Position = NHDPlus total drainage area (larger = further downstream),
#    read from the flowline GeoPackage without loading the geometry.
#    Per stream: correlation of intercept with log(drainage area) and with
#    elevation. Pooled: within-stream slope of intercept on log(drainage area).
# -----------------------------------------------------------------------------
con <- DBI::dbConnect(RSQLite::SQLite(), here("data", "processed", "gis", "flowline_pn17_nsi_MFSR.gpkg"))
nhd <- DBI::dbGetQuery(con, "select COMID, LENGTHKM, TotDASqKM from flowline_pn17_nsi_MFSR") |>
  mutate(COMID = as.character(COMID)) |>
  group_by(COMID) |>
  summarise(length_km = first(LENGTHKM), da_km2 = max(TotDASqKM), .groups = "drop")
DBI::dbDisconnect(con)

re <- ranef(final)$COMID |>
  rownames_to_column("COMID") |>
  rename(intercept = `(Intercept)`) |>
  left_join(d |> distinct(COMID = as.character(COMID), stream), by = "COMID") |>
  left_join(nhd, by = "COMID") |>
  left_join(reach_attr |> select(COMID, elev_m = elev), by = "COMID") |>
  left_join(d |> count(COMID = as.character(COMID), name = "n_redds"), by = "COMID")

message(sprintf(
  "COMID length: median %.2f km (range %.2f-%.2f), n = %d",
  median(re$length_km, na.rm = TRUE), min(re$length_km, na.rm = TRUE),
  max(re$length_km, na.rm = TRUE), sum(!is.na(re$length_km))
))

F4 <- re |>
  group_by(stream) |>
  filter(n() >= 5) |>
  summarise(
    n_comid = n(),
    r_intercept_logDA = cor(intercept, log(da_km2), use = "complete.obs"),
    r_intercept_elev = cor(intercept, elev_m, use = "complete.obs"),
    .groups = "drop"
  )
m_pos <- lmer(intercept ~ log(da_km2) + (1 | stream), data = re)
F4 <- bind_rows(F4, tibble(
  stream = "ALL (pooled within-stream slope)",
  n_comid = nrow(re),
  slope_logDA = fixef(m_pos)[[2]],
  slope_logDA_se = sqrt(vcov(m_pos)[2, 2])
))
message(sprintf(
  "Pooled within-stream slope of COMID intercept on log(drainage area): %.2f d (SE %.2f)",
  fixef(m_pos)[2], sqrt(vcov(m_pos)[2, 2])
))
print(F4)
write_csv(F4, file.path(out_tab, "F4_comid_intercepts_vs_position.csv"))
write_csv(re, file.path(out_tab, "F4_comid_random_intercepts.csv"))

# -----------------------------------------------------------------------------
# F4 plot: reach intercepts vs drainage area, by stream (Fig. A5.1)
# -----------------------------------------------------------------------------
p_pos <- ggplot(re, aes(da_km2, intercept, colour = stream)) +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_point(aes(size = n_redds)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.6) +
  scale_x_log10() +
  scale_colour_manual(values = cols.streams) +
  facet_wrap(~stream, scales = "free_x") +
  labs(
    x = "Total drainage area (km\u00B2, log scale; larger = further downstream)",
    y = "Reach random intercept (d)", size = "Redds"
  ) +
  theme(legend.position = "none")
ggsave(file.path(out_plot, "F4_comid_intercepts_vs_drainage_area.png"), p_pos, width = 8, height = 6, dpi = 300)

# -----------------------------------------------------------------------------
# F5 Elevation sensitivity. Elevation is fixed per reach, so it competes only
#    with T_bar. Big Creek spans ~650 m of surveyed elevation (others <= ~300 m).
#    (a) with vs (b) without elevation, all streams and without Big Creek;
#    then one model with separate elevation slopes for Big Creek and others.
# -----------------------------------------------------------------------------
coef_row <- function(m, v) c(fixef(m)[[v]], sqrt(vcov(m)[v, v])) # estimate, SE
F5 <- map_dfr(c("All streams", "Without Big Creek"), \(sub) {
  dd <- if (sub == "All streams") d else filter(d, stream != "Big")
  a <- fm("T_bar + T_dev + E", data = dd)
  b <- fm("T_bar + T_dev", data = dd)
  tibble(
    data = sub, n_redds = nrow(dd),
    T_bar_noE = coef_row(b, "T_bar")[1], T_bar_noE_se = coef_row(b, "T_bar")[2],
    T_bar_withE = coef_row(a, "T_bar")[1], T_bar_withE_se = coef_row(a, "T_bar")[2],
    T_dev_withE = coef_row(a, "T_dev")[1], T_dev_withE_se = coef_row(a, "T_dev")[2],
    E_per100m = coef_row(a, "E")[1], E_se = coef_row(a, "E")[2],
    dAIC_adding_E = AIC(a) - AIC(b)
  )
})
mb <- fm("T_bar + T_dev + E_big + E_oth",
  data = d |> mutate(E_big = E * (stream == "Big"), E_oth = E * (stream != "Big"))
)
F5 <- bind_rows(F5, tibble(
  data = "All streams, elevation slope split",
  E_big = coef_row(mb, "E_big")[1], E_big_se = coef_row(mb, "E_big")[2],
  E_others = coef_row(mb, "E_oth")[1], E_others_se = coef_row(mb, "E_oth")[2]
))
elev_span <- d |>
  distinct(COMID, stream, elev) |>
  group_by(stream) |>
  summarise(elev_min = min(elev), elev_max = max(elev), span_m = elev_max - elev_min, n_reaches = n())
print(F5, width = Inf)
print(elev_span)
write_csv(F5, file.path(out_tab, "F5_elevation_sensitivity.csv"))
write_csv(elev_span, file.path(out_tab, "F5_elevation_span_by_stream.csv"))

