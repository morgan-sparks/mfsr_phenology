library(tidyverse)

# load data
dat <- read_csv(here::here("data", "russ_spawn", "mfsr_spawn_cleaned.csv"))


tmp <- dat |> group_by(year, stream) |> summarise(yday_50 = median(yday))

tmp |> ggplot(aes(yday_50, stream)) + geom_point()