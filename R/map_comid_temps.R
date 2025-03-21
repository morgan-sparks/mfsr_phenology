library(tidyverse)
library(purrr)

temperature_data <- readRDS(here::here("./data/siegel_temperature/siegel_mfsr_comid.RDS"))
spawn_data <- read_csv(here::here("./data/russ_spawn/mfsr_spawn_cleaned.csv"))


comid <- 23519529
redd_id <- 1
spawn_date <- "08312001"
day_spread <- 90
period <- "before"

dat <- spawn_data |>
  select(comid = COMID, redd_id = UNIQUE_ID, spawn_date = DATE) 
dat


dat2 <- dat |> 
  tidyr::expand_grid(days_spead = c(30, 60, 90), 
                     period = c("before", "span", "after"))

args <- list(
  comid = dat2$comid,
  redd_id = dat2$redd_id,
  spawn_date = dat2$spawn_date,
  day_spread = dat2$days_spead,
  period = dat2$period
)

out <- args |> pmap(
  comid_temperature,
  temperature_data = temperature_data
) |>
  map_df(~ as.data.frame(.x))

# save out to .rdata
save(out, file = here::here("./data/comid_temps.RData"))

out |> 
  mutate(date = yday(spawn_date)) |> 
  filter(period == "before", duration == 90) |>
  ggplot(aes(x = avg_temp, y = date)) +
  geom_point(size = 0.5, alpha = 0.25) + 
  labs(x = "Average Temperature (°C)", 
       y = "Day of Year (DOY)",
       title = "Average Temperature at Redd Locations 90 days before") +
  theme_classic()


out |> 
  mutate(duration = case_when(
    duration == 30 ~ "30 days",
    duration == 60 ~ "60 days",
    duration == 90 ~ "90 days"
  )) |>
  mutate(date = yday(spawn_date)) |> 
  ggplot(aes(x = avg_temp, y = date, color = period)) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~as.factor(duration), ncol = 1) +
  geom_point(size = 0.5, alpha = 0.25) + 
  labs(x = "Average Temperature (°C)", 
       y = "Putative Spawn Day of Year (DOY)",
       title = "Average Temperature at Redd Locations") +
  theme_classic()


out_wide <- out |> 
  as_tibble() |> 
  pivot_wider(names_from = period, values_from = avg_temp) 

out_wide |> 
  ggplot(aes(x = before, y = after)) +
  facet_wrap(~as.factor(duration), ncol = 1) +
  geom_point() 
