# =============================================================================
# revision_setup.R  (CJFAS revision helper; sourced by R/07_* - R/11_*)
#
# Loads redds and Siegel et al. (2023) daily temperatures with the same
# filters as analysis/_shared.R, and provides win_mean(key, end_doy, n):
# mean modeled temperature for COMID-year `key` over the n days ending on
# day-of-year `end_doy` (missing days ignored; identical to
# R/04_comid-temperature.R with period = "before").
#
# Also holds the chosen thermal index (INDEX_END_DOY, INDEX_N), the model
# dataset built from it (model_data()), and the manuscript ggplot theme.
# =============================================================================

library(tidyverse)
library(here)

out_tab  <- here("tables", "revision")
out_plot <- here("plots", "revision")
dir.create(out_tab,  showWarnings = FALSE, recursive = TRUE)
dir.create(out_plot, showWarnings = FALSE, recursive = TRUE)

# Thermal index used in the final model (chosen in R/08; change here only) ----
INDEX_END_DOY <- 197   # 16 Jul (DOY in 2002; non-leap years 2002-2005)
INDEX_N       <- 60    # window length (days): 18 May - 16 Jul

doy_lab <- function(d) format(as.Date(d - 1, "2002-01-01"), "%d %b")
index_label <- sprintf("%d-d mean ending %s (%s - %s)", INDEX_N, doy_lab(INDEX_END_DOY),
                       doy_lab(INDEX_END_DOY - INDEX_N + 1), doy_lab(INDEX_END_DOY))

# Manuscript theme and palettes (theme copied from analysis/_shared.R) -------
source(here("R", "palettes-themes.R"))   # cols.streams, cols.years, fixed_date
theme_mfsr <- function(base_size = 11, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.background  = element_rect(fill = "white", color = NA),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      axis.line         = element_line(color = "black", linewidth = 0.5),
      axis.ticks        = element_line(color = "black", linewidth = 0.4),
      axis.text         = element_text(size = rel(0.85), color = "grey20"),
      axis.title        = element_text(size = rel(0.95), color = "grey10"),
      strip.background  = element_rect(fill = "white", color = "black", linewidth = 0.5),
      strip.text        = element_text(size = rel(0.9), margin = margin(3, 3, 3, 3)),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      legend.title      = element_text(size = rel(0.85)),
      legend.text       = element_text(size = rel(0.8)),
      plot.margin       = margin(4, 6, 4, 6)
    )
}

# Data (same filtering as analysis/_shared.R) ---------------------------------
redds <- read_csv(here("data", "processed", "russ_spawn", "mfsr_spawn_cleaned.csv"),
                  show_col_types = FALSE) |>
  filter(stream != "Knapp" & stream != "Cape Horn" & year != 2001) |>
  mutate(spawn_date = mdy(DATE),
         COMID  = as.character(COMID),
         year   = as.integer(year),
         stream = factor(stream)) |>
  select(redd_id = UNIQUE_ID, COMID, stream, year, spawn_date, yday)

temps <- readRDS(here("data", "processed", "siegel_temperature", "siegel_mfsr_comid.RDS")) |>
  transmute(COMID = as.character(COMID),
            date  = as.Date(tim.date),
            temp  = prd.stream_temp) |>
  mutate(year = year(date), doy = yday(date)) |>
  filter(year %in% 2002:2005, COMID %in% redds$COMID)

# -----------------------------------------------------------------------------
# Fast window means: cumulative sums per COMID-year over day of year.
# win_mean(key, end_doy, n) = mean temp over [end_doy - n + 1, end_doy],
# ignoring missing days (equivalent to R/04_comid-temperature.R).
# -----------------------------------------------------------------------------
keys <- temps |> distinct(COMID, year) |> mutate(key = paste(COMID, year, sep = "_"))
M <- matrix(NA_real_, nrow = nrow(keys), ncol = 366,
            dimnames = list(keys$key, NULL))
M[cbind(match(paste(temps$COMID, temps$year, sep = "_"), keys$key), temps$doy)] <- temps$temp
cs_val <- t(apply(ifelse(is.na(M), 0, M), 1, cumsum))
cs_n   <- t(apply(!is.na(M), 1, cumsum))
cs_val <- cbind(0, cs_val); cs_n <- cbind(0, cs_n)   # column j+1 = through doy j

win_mean <- function(key, end_doy, n) {
  i  <- match(key, rownames(M))
  e  <- end_doy + 1
  s  <- pmax(end_doy - n, 0) + 1
  (cs_val[cbind(i, e)] - cs_val[cbind(i, s)]) / (cs_n[cbind(i, e)] - cs_n[cbind(i, s)])
}

redds <- redds |>
  mutate(key     = paste(COMID, year, sep = "_"),
         temp_30 = win_mean(key, yday, 30),
         temp_60 = win_mean(key, yday, 60),
         temp_90 = win_mean(key, yday, 90)) |>
  filter(!is.na(temp_90))

# -----------------------------------------------------------------------------
# Daily series by COMID across years (2002-2006), for windows that cross
# 1 January (post-spawn / incubation indices) and for emergence predictions.
# win_mean_dates(COMID, start, end): mean temp over [start, end], any dates.
# temp_series(COMID, start, end): the daily vector itself.
# -----------------------------------------------------------------------------
temps_all <- readRDS(here("data", "processed", "siegel_temperature", "siegel_mfsr_comid.RDS")) |>
  transmute(COMID = as.character(COMID), date = as.Date(tim.date), temp = prd.stream_temp) |>
  filter(date >= as.Date("2002-01-01"), date <= as.Date("2006-12-31"), COMID %in% redds$COMID)
all_dates <- seq(as.Date("2002-01-01"), as.Date("2006-12-31"), by = "day")
Mall <- matrix(NA_real_, nrow = n_distinct(temps_all$COMID), ncol = length(all_dates),
               dimnames = list(sort(unique(temps_all$COMID)), NULL))
Mall[cbind(match(temps_all$COMID, rownames(Mall)), match(temps_all$date, all_dates))] <- temps_all$temp
cs_all_v <- cbind(0, t(apply(ifelse(is.na(Mall), 0, Mall), 1, cumsum)))
cs_all_n <- cbind(0, t(apply(!is.na(Mall), 1, cumsum)))

win_mean_dates <- function(comid, start, end) {
  i <- match(comid, rownames(Mall))
  s <- match(as.Date(start), all_dates); e <- match(as.Date(end), all_dates) + 1
  (cs_all_v[cbind(i, e)] - cs_all_v[cbind(i, s)]) / (cs_all_n[cbind(i, e)] - cs_all_n[cbind(i, s)])
}
temp_series <- function(comid, start, end) {
  Mall[comid, match(as.Date(start), all_dates):match(as.Date(end), all_dates)]
}

# Reach attributes: mean elevation (m) and slope, as in analysis/_shared.R ------
reach_attr <- readRDS(here("data", "processed", "elevslope.rds")) |>
  as_tibble() |>
  transmute(COMID = as.character(COMID),
            elev  = (MAXELEVSMO + MINELEVSMO) / 2 / 100,   # cm -> m
            slope = ifelse(SLOPE > 0.2, 0.002, SLOPE)) |>
  distinct(COMID, .keep_all = TRUE)

# -----------------------------------------------------------------------------
# Model dataset for the chosen index (used by R/09-R/11)
#   T     : reach-year thermal index (same value for every redd in a reach-year)
#   T_bar : reach mean of T over the years it was sampled (between-reach part)
#   T_dev : reach-year anomaly, T - T_bar (within-reach, between-year part)
#   E     : reach elevation, centred, in units of 100 m
#   Ts    : T scaled (for spline/quadratic fits); t90s: scaled temp_90 (reference only)
# -----------------------------------------------------------------------------
model_data <- function(end_doy = INDEX_END_DOY, n = INDEX_N) {
  d <- redds |>
    mutate(T = win_mean(key, end_doy, n)) |>
    filter(!is.na(T)) |>
    left_join(reach_attr, by = "COMID") |>
    group_by(COMID) |>
    mutate(T_bar = mean(T[!duplicated(key)])) |>
    ungroup() |>
    mutate(T_dev = T - T_bar,
           year  = factor(year),
           COMID = factor(COMID),
           key   = factor(key))
  d |> mutate(Ts   = (T - mean(T)) / sd(T),
              t90s = (temp_90 - mean(temp_90)) / sd(temp_90),
              E    = (elev - mean(elev)) / 100,
              slope_s = (slope - mean(slope)) / sd(slope))
}

# Final model formula (chosen 2 Nov 2026; see R/09 F1, F5) ------------------
# Elevation is tested as a sensitivity analysis (F5): its effect is driven by
# Big Creek's long elevational gradient and is not supported network-wide.
FINAL_FORMULA <- yday ~ T_bar + T_dev + stream + year + (1 | COMID) + (1 | key)

