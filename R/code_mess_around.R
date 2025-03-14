library(tidyverse); library(ggridges)

mfsr_spawn <- read.csv("./data/russ_spawn/mfsr_spawn_combined.csv")



max(na.omit(mfsr_spawn$DATE))
min(na.omit(mfsr_spawn$DATE))


mfsr_spawn |> 
 drop_na(REACH) |> 
  mutate(across(DATE, as.character)) |> 
  mutate(date = mdy(gsub('.*^','0',DATE))) |>
  mutate(year = year(date)) |> 
  mutate(yday = yday(date)) |> 
  ggplot(aes(x = yday, y = as.character(year))) +
  geom_density_ridges(scale = 1.1) +
  labs(x = "Day of Year", y = "Year") +
  theme_classic()

mfsr_spawn |> 
  drop_na(REACH) |> 
  mutate(across(DATE, as.character)) |> 
  mutate(date = mdy(gsub('.*^','0',DATE))) |>
  mutate(year = year(date)) |> 
  mutate(yday = yday(date)) |>
  mutate(month = month(date)) |> 
  filter(OBJ_CLASS %in% c("Redd", "REDD")) |> 
  group_by(year) |> 
  tally()
