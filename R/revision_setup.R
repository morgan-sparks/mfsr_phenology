# =============================================================================
# revision_setup.R  (CJFAS revision helper; sourced by R/07_* and R/08_*)
#
# Loads redds and Siegel et al. (2023) daily temperatures with the same
# filters as analysis/_shared.R, and provides win_mean(key, end_doy, n):
# mean modeled temperature for COMID-year `key` over the n days ending on
# day-of-year `end_doy` (missing days ignored; identical to
# R/04_comid-temperature.R with period = "before").
# =============================================================================

library(tidyverse)
library(here)

out_tab  <- here("tables", "revision")
out_plot <- here("plots", "revision")
dir.create(out_tab,  showWarnings = FALSE, recursive = TRUE)
dir.create(out_plot, showWarnings = FALSE, recursive = TRUE)

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

