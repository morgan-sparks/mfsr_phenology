# =============================================================================
# 12_long-term-thermal-context.R  (CJFAS revision)
#
# How do the four study years (2002-2005) compare with the full record of
# modeled stream temperature (Siegel et al. 2023; 1990-2021)?
#
# The thermal index (60-d mean, 18 May - 16 Jul) is computed for every
# surveyed reach in every year of the record. Three questions:
#   G1  Were 2002-2005 typical years for the network as a whole?
#   G2  Is the ranking of reaches (the between-reach contrast, T_bar) stable
#       across years, i.e. is the index a property of the reach?
#   G3  How large are the reach-year anomalies (T - T_bar, the within-reach
#       contrast) in the study years compared with the full record? This sets
#       the range over which the within-reach slope can be interpreted.
#
# Outputs (tables/revision/, plots/revision/):
#   G1_index_by_year.csv, G1_network_index_by_year.png
#   G2_variance_partition.csv, G2_reach_rank_stability.csv
#   G3_anomaly_summary.csv, G3_anomaly_distribution.png
# =============================================================================

source(here::here("R", "revision_setup.R")) # siegel (all years), index settings, std_doy()
theme_set(theme_mfsr())

study_years <- 2002:2005

# Thermal index for every surveyed reach in every year of the record. Using
# std_doy() keeps the window on the same calendar days (18 May - 16 Jul) in
# leap years, exactly as in model_data().
lt_index <- siegel |>
  mutate(year = year(date), doy = std_doy(date)) |>
  filter(doy > INDEX_END_DOY - INDEX_N, doy <= INDEX_END_DOY) |>
  group_by(COMID, year) |>
  summarise(T = mean(temp), n_days = n(), .groups = "drop") |>
  filter(n_days >= 55) |> # require near-complete windows
  left_join(distinct(redds, COMID, stream), by = "COMID")

n_reach <- n_distinct(lt_index$COMID)
cat("Long-term index:", n_reach, "reaches,", n_distinct(lt_index$year), "years\n")

# -----------------------------------------------------------------------------
# G1. Network-wide index by year
# Network mean = mean over all surveyed reaches (same reaches every year), so
# year-to-year differences are not driven by which reaches were sampled.
# -----------------------------------------------------------------------------
g1 <- lt_index |>
  group_by(year) |>
  summarise(
    network_mean = mean(T), q10 = quantile(T, 0.1), q90 = quantile(T, 0.9),
    .groups = "drop"
  ) |>
  mutate(
    study_year = year %in% study_years,
    anomaly = network_mean - mean(network_mean),
    pct_rank = rank(network_mean) / n()
  ) # 1 = warmest year in record
write_csv(g1, file.path(out_tab, "G1_index_by_year.csv"))

lt_mean <- mean(g1$network_mean)
p_g1 <- ggplot(g1, aes(year, network_mean)) +
  geom_hline(yintercept = lt_mean, linetype = 2, color = "grey50") +
  geom_linerange(aes(ymin = q10, ymax = q90), color = "grey70", linewidth = 0.6) +
  geom_line(color = "grey40", linewidth = 0.4) +
  geom_point(aes(fill = study_year), shape = 21, size = 2.3) +
  scale_fill_manual(
    values = c(`FALSE` = "white", `TRUE` = "black"),
    labels = c("Other years", "Study years"), name = NULL
  ) +
  labs(x = NULL, y = "Thermal index (\u00B0C)") +
  theme(legend.position = c(0.12, 0.92))
ggsave(file.path(out_plot, "G1_network_index_by_year.png"), p_g1, width = 6.5, height = 3.2, dpi = 300)

# -----------------------------------------------------------------------------
# G2. Variance partition of the index: reach + year + reach-by-year
# Two-way decomposition on the complete reach x year table:
#   T_ry = grand mean + reach_r + year_y + resid_ry
# The residual (reach-by-year) term is the part of a reach's anomaly that is
# not shared by the whole network in that year. With year fixed effects in the
# spawn-timing model, this is the variation that identifies the within-reach
# slope.
# -----------------------------------------------------------------------------
partition <- function(df) {
  df <- df |>
    group_by(COMID) |>
    mutate(reach_eff = mean(T)) |>
    ungroup() |>
    group_by(year) |>
    mutate(year_eff = mean(T)) |>
    ungroup() |>
    mutate(
      gm = mean(T),
      reach_eff = reach_eff - gm, year_eff = year_eff - gm,
      resid = T - gm - reach_eff - year_eff
    )
  v <- c(reach = var(df$reach_eff), year = var(df$year_eff), reach_x_year = var(df$resid))
  tibble(component = names(v), variance = v, share = v / sum(v), sd = sqrt(v))
}
complete_reaches <- lt_index |>
  count(COMID) |>
  filter(n == max(n)) |>
  pull(COMID)
g2 <- bind_rows(
  partition(filter(lt_index, COMID %in% complete_reaches)) |> mutate(period = "1990-2021"),
  partition(filter(lt_index, COMID %in% complete_reaches, year %in% study_years)) |>
    mutate(period = "2002-2005")
)
write_csv(g2, file.path(out_tab, "G2_variance_partition.csv"))

# Rank stability: correlation of reach index between every pair of years
wide <- lt_index |>
  filter(COMID %in% complete_reaches) |>
  select(COMID, year, T) |>
  pivot_wider(names_from = year, values_from = T)
cm <- cor(as.matrix(wide[, -1]), method = "spearman")
g2_rank <- tibble(
  comparison = c(
    "All pairs of years, 1990-2021", "Pairs within 2002-2005",
    "Study-year mean vs 1990-2021 mean"
  ),
  spearman = c(
    median(cm[upper.tri(cm)]),
    median(cm[as.character(study_years), as.character(study_years)][upper.tri(diag(4))]),
    cor(rowMeans(as.matrix(wide[, as.character(study_years)])),
      rowMeans(as.matrix(wide[, -1])),
      method = "spearman"
    )
  ),
  min = c(
    min(cm[upper.tri(cm)]),
    min(cm[as.character(study_years), as.character(study_years)][upper.tri(diag(4))]),
    NA
  )
)
write_csv(g2_rank, file.path(out_tab, "G2_reach_rank_stability.csv"))

# -----------------------------------------------------------------------------
# G3. Reach-year anomalies: study years vs the full record
# Three anomaly definitions, from the model's to the long-term view:
#   model   : T - T_bar, with T_bar the reach mean over the years the reach was
#             surveyed (exactly the T_dev used in the model; redd-weighted rows
#             collapsed to one per reach-year)
#   lt_raw  : T - long-term reach mean (1990-2021); includes network-wide
#             warm/cool years
#   lt_rxy  : reach-by-year residual from G2 (network-wide year effect removed);
#             the part comparable to T_dev once year fixed effects are in the model
# -----------------------------------------------------------------------------
md <- model_data() |>
  distinct(key, COMID, year, T_dev) |>
  mutate(COMID = as.character(COMID), year = as.integer(as.character(year)))

lt_anom <- lt_index |>
  group_by(COMID) |>
  mutate(lt_raw = T - mean(T)) |>
  ungroup() |>
  group_by(year) |>
  mutate(yr_eff = mean(T)) |>
  ungroup() |>
  mutate(lt_rxy = lt_raw - (yr_eff - mean(T)))

g3 <- bind_rows(
  md |> transmute(set = "Model anomaly (T - T_bar), surveyed reach-years", a = T_dev),
  lt_anom |> filter(year %in% study_years, paste(COMID, year) %in% paste(md$COMID, md$year)) |>
    transmute(set = "Long-term anomaly, surveyed reach-years", a = lt_raw),
  lt_anom |> transmute(set = "Long-term anomaly, all reach-years 1990-2021", a = lt_raw),
  lt_anom |> filter(year %in% study_years, paste(COMID, year) %in% paste(md$COMID, md$year)) |>
    transmute(set = "Reach-by-year residual, surveyed reach-years", a = lt_rxy),
  lt_anom |> transmute(set = "Reach-by-year residual, all reach-years 1990-2021", a = lt_rxy)
) |>
  group_by(set) |>
  summarise(
    n = n(), sd = sd(a), q05 = quantile(a, 0.05), q95 = quantile(a, 0.95),
    min = min(a), max = max(a), .groups = "drop"
  )
write_csv(g3, file.path(out_tab, "G3_anomaly_summary.csv"))

# (A) anomalies from the long-term reach mean; (B) the reach-by-year part
#     only (network-wide year effect removed). Study years vs other years.
p_g3 <- lt_anom |>
  mutate(period = factor(ifelse(year %in% study_years, "2002-2005", "Other years"),
    levels = c("Other years", "2002-2005")
  )) |>
  pivot_longer(c(lt_raw, lt_rxy), names_to = "type", values_to = "a") |>
  mutate(type = recode(type,
    lt_raw = "(A) Anomaly from the long-term reach mean",
    lt_rxy = "(B) Reach-specific part (network-wide year effect removed)"
  )) |>
  ggplot(aes(a, fill = period)) +
  geom_density(alpha = 0.6, color = NA, adjust = 1.2) +
  geom_vline(xintercept = 0, linetype = 3) +
  facet_wrap(~type, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("Other years" = "grey75", "2002-2005" = "grey20"), name = NULL) +
  labs(x = "Reach-year anomaly in thermal index (\u00B0C)", y = "Density") +
  theme(legend.position = "top")
ggsave(file.path(out_plot, "G3_anomaly_distribution.png"), p_g3, width = 6, height = 5, dpi = 300)

print(g1 |> filter(study_year))
print(g2)
print(g2_rank)
print(g3)
