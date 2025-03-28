library(tidyverse)

temp_data <- readRDS(here::here("./data/siegel_temperature/siegel_mfsr_comid.RDS"))
spawn_data <- read_csv(here::here("./data/russ_spawn/mfsr_spawn_cleaned.csv"))

# get streams to add to temp_data
spawn_reduced <- spawn_data |> 
  select(COMID, stream) |>  
  group_by(COMID) |> 
  slice_head(n =1)

# make df to plot
temp_data_sum <- temp_data |> 
  left_join(y = spawn_reduced, by = "COMID") |> # add streams
  filter(stream != "Knapp" & stream != "Cape Horn") |>  #remove Knapp and Cape Horn
  filter(tim.date >= ymd("2002-07-01") & tim.date <= ymd("2006 -07-01")) |> # subset to spawning years
  mutate(doy = yday(tim.date)) |>  # make FOY
  #group_by(COMID, doy, stream ) |> # comid grouping
  group_by(stream, doy) |> 
  summarise (mean_temp = mean(prd.stream_temp), # make summaries by groupings
             min_temp = min(prd.stream_temp),
             max_temp = max(prd.stream_temp)) |> 
  ungroup() |> 
  mutate(date = case_when(doy >= 182 ~ as.Date("2021-12-31") + doy, # make a fake year column for plotting
                          doy < 182 ~ as.Date("2022-12-31") + doy)) # conditional because spans two calendar years

#plot temps for each stream (can do for COMID too if change grouping above)
temp_data_sum |> 
  ggplot() +
  #geom_ribbon(aes(x =date, ymin = min_temp, ymax = max_temp, fill = stream), alpha = 0.55, ) +
  geom_line(aes(x = date, y = mean_temp, color = stream ), size = 0.5) +
  scale_x_date(labels = scales::date_format("%b")) +
 #facet_wrap(~stream) +
  theme_classic()
