library(tidyverse); library(here)

# Import data -------------------------------------------------------------

spawn_data <- read_csv(here("./data/russ_spawn/mfsr_spawn_cleaned.csv"))
flow_data <- read_csv(here("./data/spawn_flows.csv"))
comid_data <- readRDS(here("./data/elevslope.rds"))
load("~/Library/CloudStorage/Box-Box/Morgan.Sparks/Projects/mfsr_phenology/data/comid_temps.RData")

# Combine data ------------------------------------------------------------

temp_data <- out |> 
  filter(period == "before" & duration %in% c(30,60,90)) |> 
  pivot_wider(names_from = "duration", values_from = avg_temp, names_prefix = "temp_")

combined_data  <- spawn_data |> 
  left_join(flow_data, join_by(date == spawn_date))

combined_data  <- combined_data  |> 
  left_join(temp_data, join_by( UNIQUE_ID == redd_id))

combined_data  <- combined_data  |> 
  left_join(comid_data, join_by(COMID)) |> 
  mutate(mean_elevation = (MAXELEVSMO + MINELEVSMO)/2/100,
         yday = yday(date)) |> 
  filter( stream != "Knapp" & stream != "Cape Horn")


# Models ------------------------------------------------------------------


## Full Model --------------------------------------------------------------

#full mod
full_mod <- lm(yday ~  flow_30 + flow_60 + flow_90 + 
           temp_30 + temp_60 + temp_90 + 
           mean_elevation + SLOPE + stream, data = combined_data )


sjPlot::tab_model(full_mod)

# full mod without colinear predictors
full_mod_nocor <- lm(yday ~  flow_30 + temp_90 + mean_elevation + SLOPE + stream, data = combined_data )

sjPlot::tab_model(full_mod)
## Interaction Model -------------------------------------------------------


int_mod <- lm(yday ~  flow_30 +
                   temp_90*stream + 
                   mean_elevation + 
                   SLOPE, data = combined_data )

sjPlot::tab_model(int_mod)

GGally::ggpairs(combined_data, 19:21)


## AIC ---------------------------------------------------------------------
extractAIC(full_mod); extractAIC(int_mod); extractAIC(full_mod_nocor)



