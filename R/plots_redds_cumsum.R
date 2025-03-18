library(tidyverse)

# load data
dat <- read_csv(here::here("data", "russ_spawn", "mfsr_spawn_cleaned.csv"))

# clean up
dat <- dat |> 
  select(-OBS_CLASS, -OBS_NAME, -Feature_ID, -FEATURES) |> 
  filter(year != 2001) |> 
  filter(! stream %in% c("Knapp", "Cape Horn"))

# For each year and stream, calculate the prop. cumulative number of redds
tmp <- dat |> 
  select(year, stream, yday) |>
  group_by(year, stream, yday) |>
  add_count() |> 
  distinct() |> 
  group_by(year, stream) |>
  arrange(year, stream, yday) |>
  mutate(cum_redds = cumsum(n)) |> 
  mutate(cum_redds_p = cum_redds / max(cum_redds))
tmp

# Plot pro. cumsums
p.cumsum <- tmp |> 
  ggplot(aes(x = yday, y = cum_redds_p, color = as.factor(year))) +
  lemon::facet_rep_wrap(~stream, ncol = 1, scales = "free_y") +
  # facet_wrap(~year, ncol = 1, scales = "free_y") +
  geom_line(linewidth = 1) + 
  geom_point() + 
  theme_classic() + 
  scale_color_brewer(palette = "Dark2") + 
  labs(
    x = "Day of Year (Julian)",
    y = "Proportional Cumulative Redds",
    color = ""
  )
p.cumsum

# save plot
ggsave(
  here::here("plots", "redd_cumsum.png"), 
  p.cumsum, 
  height = 11, 
  width = 3)
