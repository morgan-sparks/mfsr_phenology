# Shared data loading for analysis fragments and appendix
# Source this file before running any analysis chunk interactively:
#   source(here::here("analysis/_shared.R"))
# The appendix.qmd sources this automatically in its setup chunk.

library(tidyverse)
library(here)
library(lme4)
library(splines)

source(here("R/utils.R"))
source(here("R/palettes-themes.R"))

# Spawn data ---------------------------------------------------------------
spawn_data_cleaned <- read_csv(here("data", "processed", "russ_spawn", "mfsr_spawn_cleaned.csv"))
coords <- read_csv(here("data", "processed", "df_sf.csv"))

spawn_data <- spawn_data_cleaned |>
  filter(stream != "Knapp" & stream != "Cape Horn" & year != 2001) |>
  mutate(
    UNIQUE_ID  = as.factor(UNIQUE_ID),
    year       = as.factor(year),
    stream     = as.factor(stream),
    COMID      = as.factor(COMID),
    DATE       = mdy(DATE)
  ) |>
  select(redd_id = UNIQUE_ID, COMID, spawn_date = DATE, stream, year, yday)

# Cross-reference COMID <-> stream
xref_comid_stream <- spawn_data |> distinct(COMID, stream)

# Temperature data ---------------------------------------------------------
df_temp <- readRDS(here("data", "processed", "siegel_temperature", "siegel_mfsr_comid.RDS"))

df_temp <- df_temp |>
  select(COMID, date = tim.date, temp = prd.stream_temp) |>
  mutate(
    yday  = yday(date),
    year  = as_factor(year(date)),
    COMID = as.character(COMID)
  ) |>
  filter(COMID %in% spawn_data$COMID) |>
  filter(year %in% c("2002", "2003", "2004", "2005")) |>
  left_join(xref_comid_stream, by = "COMID")

# Summarised temperature data (computed in R/04_comid-temperature.R)
load(here("data", "processed", "comid_temps.RData"))

temp_data <- out |>
  filter(period == "before" & duration %in% c(30, 60, 90)) |>
  pivot_wider(names_from = "duration", values_from = avg_temp, names_prefix = "temp_") |>
  select(redd_id, COMID = comid, spawn_date, temp_30, temp_60, temp_90) |>
  mutate(redd_id = as_factor(redd_id))

# Flow data ----------------------------------------------------------------
df_flows <- read_csv(here("data", "processed", "mfsr_flow.csv")) |>
  select(date = Date, flow_cfs = Flow) |>
  mutate(
    yday     = yday(date),
    year     = as_factor(year(date)),
    flow_csm = flow_cfs * 0.028316846592
  ) |>
  filter(!year == "2001")

flow_data <- read_csv(here("data", "processed", "spawn_flows.csv"))

# Elevation and slope ------------------------------------------------------
df_elev_slope <- readRDS(here("data", "processed", "elevslope.rds")) |>
  as_tibble() |>
  mutate(mean_elevation = (MAXELEVSMO + MINELEVSMO) / 2 / 100) |>
  select(COMID, slope = SLOPE, mean_elevation)

# Combined model dataset ---------------------------------------------------
model_data <- spawn_data |>
  left_join(flow_data, by = "spawn_date") |>
  left_join(temp_data |> select(-spawn_date, -COMID), by = "redd_id") |>
  left_join(df_elev_slope |> mutate(COMID = as.factor(COMID)), by = "COMID") |>
  mutate(slope = ifelse(slope > 0.2, 0.002, slope))

# Scale continuous predictors (mean = 0, SD = 1)
df_mod <- model_data |>
  select(yday, COMID, stream, year, temp_90, mean_elevation, slope) |>
  mutate(
    temp_90       = scale2(temp_90),
    mean_elevation = scale2(mean_elevation),
    slope         = scale2(slope)
  )

# Back-transform constants for plotting
mean_temp <- mean(model_data$temp_90, na.rm = TRUE)
sd_temp   <- sd(model_data$temp_90,   na.rm = TRUE)

df_mod_plot <- df_mod |>
  mutate(temp_90_raw = temp_90 * sd_temp + mean_temp)
