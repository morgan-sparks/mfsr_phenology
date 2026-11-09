# =============================================================================
# revision_setup.R  (CJFAS revision; sourced by R/07-R/12 and the appendix)
#
# One place for everything the revision scripts share:
#   1. Settings   : output folders, the chosen thermal index, model formula
#   2. Data       : redds and Siegel et al. (2023) modeled daily temperatures
#   3. Windows    : fast window means over the daily temperature series
#   4. Model data : model_data() builds T, T_bar, T_dev and covariates
#   5. Helpers    : model fitting/summary and plotting helpers
#
# Calendar convention. All day-of-year (DOY) values use a NON-LEAP calendar:
# in 2004, dates after 28 Feb are shifted back one day and 29 Feb is dropped,
# so a given calendar date has the same DOY in every year (16 Jul = 197).
# This applies to spawn dates (yday) and to the temperature series, so fixed-
# date windows cover the same calendar days each year and fixed_date + yday
# plots the correct date. (The submitted analysis used raw DOY, which put 2004
# one day out of step.)
# =============================================================================

library(tidyverse)
library(here)
source(here("R", "palettes-themes.R"))   # cols.streams, cols.years, fixed_date, theme_mfsr()

# -----------------------------------------------------------------------------
# 1. Settings
# -----------------------------------------------------------------------------
out_tab <- here("tables", "revision")
out_plot <- here("plots", "revision")
dir.create(out_tab, showWarnings = FALSE, recursive = TRUE)
dir.create(out_plot, showWarnings = FALSE, recursive = TRUE)

# Thermal index used in the final model (chosen in R/08; change it only here)
INDEX_END_DOY <- 197 # 16 Jul
INDEX_N <- 60 # window length in days: 18 May - 16 Jul

# DOY on the non-leap calendar, and labels for it
std_doy <- function(date) {
  date <- as.Date(date)
  yday(date) - (leap_year(date) & month(date) > 2)
}
doy_lab <- function(d) format(fixed_date + d, "%d %b") # fixed_date is 31 Dec 2000
index_label <- sprintf(
  "%d-d mean ending %s (%s - %s)",
  INDEX_N,
  doy_lab(INDEX_END_DOY),
  doy_lab(INDEX_END_DOY - INDEX_N + 1),
  doy_lab(INDEX_END_DOY)
)

# Final model (chosen 2 Nov 2026; R/09 F1 and F5, Table 1). Elevation is a
# sensitivity analysis only: its effect comes from Big Creek's gradient.
FINAL_FORMULA <- yday ~ T_bar + T_dev + stream + year + (1 | COMID) + (1 | key)

# Optimizer used for every lmer fit
ctl <- lme4::lmerControl(optimizer = "bobyqa")


# -----------------------------------------------------------------------------
# 2. Data
# Redds: same filters as the submitted analysis (drop Knapp and Cape Horn
# creeks and 2001, which were not surveyed consistently).
# Temperatures: Siegel et al. (2023) daily predictions for surveyed reaches.
# -----------------------------------------------------------------------------
redds <- read_csv(
  here("data", "processed", "russ_spawn", "mfsr_spawn_cleaned.csv"), 
  show_col_types = FALSE
  ) |>
  filter(stream != "Knapp" & stream != "Cape Horn" & year != 2001) |>
  mutate(
    spawn_date = mdy(DATE),
    yday = std_doy(spawn_date), # response: spawn DOY (non-leap calendar)
    COMID = as.character(COMID),
    year = as.integer(year),
    stream = factor(stream),
    key = paste(COMID, year, sep = "_")  # reach-year identifier
  ) |> 
  select(redd_id = UNIQUE_ID, COMID, stream, year, key, spawn_date, yday)

siegel <- readRDS(
  here("data", "processed", "siegel_temperature", "siegel_mfsr_comid.RDS")
  ) |>
  transmute(
    COMID = as.character(COMID), 
    date = as.Date(tim.date), 
    temp = prd.stream_temp
    ) |>
  filter(COMID %in% redds$COMID, format(date, "%m-%d") != "02-29")

# Study years only, indexed by non-leap DOY
temps <- siegel |>
  filter(year(date) %in% 2002:2005) |>
  mutate(year = year(date), doy = std_doy(date))

# -----------------------------------------------------------------------------
# 3. Window means - running total method
# For our artifact diagnostics, we need means over many windows (redds x windows x
# 200 permutations in R/07), so means are computed from cumulative sums rather
# than by filtering the daily data each time:
#   mean over days s to e = (cumsum[e] - cumsum[s-1]) / (number of non-missing days)
# Missing days are skipped, matching original R/04_comid-temperature.R.
# -----------------------------------------------------------------------------
# Turn reach x day temp matrix into two matricies of same shape
cumsum_matrix <- function(M) {
  list(
    v = cbind(0, t(apply(ifelse(is.na(M), 0, M), 1, cumsum))), # running sum of temps
    n = cbind(0, t(apply(!is.na(M), 1, cumsum)))
  ) # running count of days
}

# Vectorized look-ups; index by row and column pairs, so no loop needed
window_from_cumsum <- function(cs, i, s, e) { # rows i, columns s..e (1-based, inclusive)
  (cs$v[cbind(i, e + 1)] - cs$v[cbind(i, s)]) / (cs$n[cbind(i, e + 1)] - cs$n[cbind(i, s)])
}

# Two look-up tables ----------------
# (a) Within one year
#     Rows = reach-years (key), columns = DOY 1-365.
#     win_mean(key, end_doy, n): mean temperature over the n days ending on end_doy for each reach-year.
#     Used for spawn-date-anchored windows (end_doy = a redd's yday)
#     and for fixed-date indices (end_doy = a calendar date).
keys <- temps |>
  distinct(COMID, year) |>
  mutate(key = paste(COMID, year, sep = "_"))
M <- matrix(NA_real_, nrow = nrow(keys), ncol = 365, dimnames = list(keys$key, NULL))
M[cbind(match(paste(temps$COMID, temps$year, sep = "_"), keys$key), temps$doy)] <- temps$temp
cs_year <- cumsum_matrix(M)

win_mean <- function(key, end_doy, n) {
  window_from_cumsum(cs_year, match(key, rownames(M)), pmax(end_doy - n + 1, 1), end_doy)
}

# (b) Across years, for windows that cross 1 January (post-spawn indices, emergence).
#     Rows = reaches, columns = every date 2002-2006.
#     win_mean_dates(COMID, start, end): mean over [start, end].
#     temp_series(COMID, start, end): the daily values themselves; needed for hatchR
all_dates <- seq(as.Date("2002-01-01"), as.Date("2006-12-31"), by = "day")
all_dates <- all_dates[format(all_dates, "%m-%d") != "02-29"]
s_all <- filter(siegel, date %in% all_dates)
Mall <- matrix(NA_real_,
  nrow = n_distinct(s_all$COMID), ncol = length(all_dates),
  dimnames = list(sort(unique(s_all$COMID)), NULL)
)
Mall[cbind(match(s_all$COMID, rownames(Mall)), match(s_all$date, all_dates))] <- s_all$temp
cs_all <- cumsum_matrix(Mall)

# date lookup: converts date to its column; 29 Feb maps to 28 Feb
date_col <- function(x) {
  x <- as.Date(x)
  x[format(x, "%m-%d") == "02-29"] <- x[format(x, "%m-%d") == "02-29"] - 1
  match(x, all_dates)
}
win_mean_dates <- function(comid, start, end) {
  window_from_cumsum(cs_all, match(comid, rownames(Mall)), date_col(start), date_col(end))
}
temp_series <- function(comid, start, end) Mall[comid, date_col(start):date_col(end)]

# Date-anchored antecedent means used in the submitted analysis
#     R/07 tests these artifacts
#     R/08 uses temp_90 as reference
redds <- redds |>
  mutate(
    temp_30 = win_mean(key, yday, 30),
    temp_60 = win_mean(key, yday, 60),
    temp_90 = win_mean(key, yday, 90)
  ) |>
  filter(!is.na(temp_90))

# -----------------------------------------------------------------------------
# 4. Model dataset
#   T       reach-year thermal index (one value per reach-year)
#   T_bar   reach mean of T over the years the reach was surveyed. Each
#           reach-year counts once (not once per redd).
#   T_dev   T - T_bar: the reach-year anomaly (within-reach, between-year)
#   E       reach elevation, centered, in 100-m units (sensitivity only)
#   slope_s reach slope, standardized (sensitivity only)
#   Ts, t90s  scaled T and temp_90, for spline fits
# -----------------------------------------------------------------------------
reach_attr <- readRDS(here("data", "processed", "elevslope.rds")) |>
  as_tibble() |>
  transmute(
    COMID = as.character(COMID),
    elev = (MAXELEVSMO + MINELEVSMO) / 2 / 100, # NHDPlus elevations are in cm
    slope = ifelse(SLOPE > 0.2, 0.002, SLOPE)
  ) |> # as in the submitted analysis
  distinct(COMID, .keep_all = TRUE)

model_data <- function(end_doy = INDEX_END_DOY, n = INDEX_N) {
  redds |>
    mutate(T = win_mean(key, end_doy, n)) |> # thermal index; one value per reach-year, for each redd
    filter(!is.na(T)) |>
    left_join(reach_attr, by = "COMID") |>
    group_by(COMID) |>
    mutate(T_bar = mean(T[!duplicated(key)])) |> # within each reach across time; each redd counts once
    ungroup() |>
    mutate(
      T_dev = T - T_bar, # reach-year anomaly
      Ts = (T - mean(T)) / sd(T),
      t90s = (temp_90 - mean(temp_90)) / sd(temp_90),
      E = (elev - mean(elev)) / 100,
      slope_s = (slope - mean(slope)) / sd(slope),
      year = factor(year),
      COMID = factor(COMID),
      key = factor(key)
    )
}

# -----------------------------------------------------------------------------
# 5. Helpers
# -----------------------------------------------------------------------------
# One-row summary of a fitted model (used for Table 1 and F1)
fit_summary <- function(m, label) {
  r2 <- suppressWarnings(performance::r2_nakagawa(m))
  tibble(
    model = label, k = attr(logLik(m), "df"), AIC = AIC(m),
    R2_m = r2$R2_marginal, R2_c = r2$R2_conditional,
    RMSE = sqrt(mean(residuals(m)^2)), singular = lme4::isSingular(m)
  )
}

# Spawn dates with the stream and year fixed effects removed, for partial-
# effect plots (Fig. 4; Fig. A4.1). Each redd's yday has its stream and year
# coefficients subtracted and the average over levels added back, which is
# the same averaging emmeans uses, so points sit on the plotted model line.
adjust_stream_year <- function(d, model) {
  fe <- lme4::fixef(model)
  level_effects <- function(f) { # coefficient per level, reference = 0
    lv <- levels(d[[f]])
    setNames(ifelse(paste0(f, lv) %in% names(fe), fe[paste0(f, lv)], 0), lv)
  }
  s <- level_effects("stream")
  y <- level_effects("year")
  d |> mutate(y_adj = yday - s[as.character(stream)] - y[as.character(year)] + mean(s) + mean(y))
}
