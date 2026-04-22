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
  geom_line(linewidth = 0.5) + 
  geom_point(size = 0.3) + 
  theme_classic() + 
  scale_color_brewer(palette = "Dark2") + 
  labs(
    x = "Day of Year",
    y = "Proportional Cumulative Redds",
    color = ""
  )
p.cumsum

# Save plot ====================================================================

path <- here::here("plots", "redd_cumsum")

ggsave(
  glue::glue("{path}.png"), 
  p.cumsum, 
  device = ragg::agg_png, 
  height = 11,
  width = 4,
  res = 300,  
  units = "in"
) 

ggsave(
  glue::glue("{path}.pdf"), 
  p.cumsum, 
  device = cairo_pdf, 
  height = 11,
  width = 4,
  units = "in"
) 
