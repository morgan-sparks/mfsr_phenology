
# Libraries ---------------------------------------------------------------


library(hatchR); library(tidyverse); library(ggridges); library(patchwork)


# Import data -------------------------------------------------------------


spawn_data <- read_csv(here::here("./data/russ_spawn/mfsr_spawn_cleaned.csv")) 

temp_data <- readRDS(here::here("./data/siegel_temperature/siegel_mfsr_comid.RDS"))



# Set up chinook model ----------------------------------------------------


chinook_emerge <- model_select(author = "Beacham and Murray 1990",
                              species = "chinook",
                              model_id = 2,
                              development_type = "emerge")

# Loop to run hatchR ------------------------------------------------------

OUT = NULL
for (R in spawn_data$UNIQUE_ID){
 
  #filter for each specific redd
  redd <- spawn_data |> filter(UNIQUE_ID == R)
  
  #filter temp_data for each redd's comid
  site <- temp_data |> filter(COMID == redd$COMID)
  
  # make a string of each redd's spawn date
  date <- redd$date |> format("%Y-%m-%d")
  
  days <- predict_phenology(data = site,
                    dates = tim.date, 
                    temperature = prd.stream_temp,
                    spawn.date = date,
                    model = chinook_emerge) 
  
  data_out <- tibble(days_to_emerge = days$days_to_develop,
         avg_emergence_temp = days$ef_table |> summarise(temp = mean(temperature)) |> pull(temp),
         redd)
  
  OUT <- rbind(OUT, data_out)
}

emergence_data <- OUT


# Add summaries to out ----------------------------------------------------


emergence_data <- emergence_data |> 
  mutate(spawn_yday = yday(date),
         emerge_yday = yday(date + days_to_emerge))

# Plots! ------------------------------------------------------------------

# Spawn DOY ~ Avg Incubation Temp (emergence)
emergence_data |> 
  ggplot(aes(x = avg_emergence_temp, y = spawn_yday, color = stream)) +
  geom_point(size =0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x= "Average incubation temp (Emergence)", y = "Spawn DOY") +
  lims(x = c(2.5,5)) +
  theme_classic()

# model for above plot
mod <- lm(spawn_yday ~ avg_emergence_temp*stream , data = emergence_data)
sjPlot::tab_model(mod)

# distribution of spawning ~ avg incubation
emergence_data |> 
  mutate(round_temp = round(avg_emergence_temp)) |> 
  ggplot()+
  geom_density_ridges(aes(x = spawn_yday, y = as.factor(round_temp), fill = stream), alpha = 0.25, scale = 0.9) +
  labs(x = "Spawn Day of Year", y = "Rounded Average Incubation Temperature (Emerge)") +
  lims(y = factor(c(3:5))) +
  theme_classic() +
    theme(legend.position="none")
  
# Days to emerge ~ Spawn DOY
emergence_data |> 
  ggplot(aes(x = spawn_yday, y = days_to_emerge, color = stream)) +
  geom_point(size =0.75) +
  geom_smooth(method = "lm", se = FALSE) +
  labs( x = "Spawn Day of Year", y ="Days to Emerge") +
  theme_classic() +
  theme(legend.position="none")

#emerge day ~ spawn day
emergence_data |> 
  filter(emerge_yday <= 300) |> 
  ggplot(aes(x = spawn_yday, y = emerge_yday, colour = stream)) +
  geom_point(size = 0.75) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic()

# calculate ranges for phenological events (at 5th and 95th quantiles)
phenology_ranges <- emergence_data |> 
  filter(stream != "Knapp" & stream != "Cape Horn") |> 
  group_by(COMID, year, stream ) |> 
  summarise(spawn_5 = quantile(spawn_yday, probs = 0.05),
            spawn_95 = quantile(spawn_yday, probs = 0.95),
            emerge_5 = quantile(emerge_yday, probs = 0.05),
            emerge_95 = quantile(emerge_yday, probs = 0.95)) |> 
  ungroup() |> 
  mutate(spawn_range = spawn_95 - spawn_5,
         emerge_range = emerge_95 - emerge_5)

# plot ranges
phenology_ranges |> 
  ggplot(aes(x = spawn_range, y = emerge_range, color = stream)) +
  geom_point(size = 0.75) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE) +
  lims(x = c(0,75), y = c(0,75)) +
  theme_classic()
