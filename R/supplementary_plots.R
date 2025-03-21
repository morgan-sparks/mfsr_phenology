library(tidyverse)

# load data
dat <- read_csv(here::here("data", "russ_spawn", "mfsr_spawn_cleaned.csv"))


# Median yday x site ------------------------------------------------------


tmp <- dat |> group_by(year, stream) |> summarise(yday_50 = median(yday))

tmp |> ggplot(aes(yday_50, stream)) + geom_point()


# Plot duration by COMID --------------------------------------------------

dat |> 
  filter(year != 2001 & stream != "Knapp" & stream != "Cape Horn") |> 
  ggplot() +
  geom_density(aes(x =yday, y = after_stat(count), group = COMID)) +
  facet_grid(stream ~ year, scales = "free_y") +
  theme_classic()

# kable table
dat |> 
  filter(year != 2001) |> 
  group_by(stream, COMID) |> 
  summarise(yday_mean = mean(yday),
            yday_sd = sd(yday),
            count = n()) |> 
  kableExtra::kbl()
