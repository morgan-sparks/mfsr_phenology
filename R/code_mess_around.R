library(tidyverse); library(ggridges)

mfsr_spawn <- read.csv("./data/russ_spawn/mfsr_spawn_combined.csv")

mfsr_spawn

x <- as.character(mfsr_spawn$DATE)

parse_date_time(na.omit(x), "%m%d%y")

visdat::vis_dat(mfsr_spawn)

visdat::vis_dat(data.frame(x))

max(na.omit(mfsr_spawn$DATE))
min(na.omit(mfsr_spawn$DATE))

mdy(gsub('.*^','0',x))

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
