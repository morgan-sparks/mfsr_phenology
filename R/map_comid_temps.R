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
  tidyr::expand_grid(days_spead = c(30, 60, 90, 120, 150, 180), 
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
#save(out, file = here::here("./data/comid_temps.RData"))

load(here::here("./data/comid_temps.RData"))

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
    duration == 90 ~ "90 days", 
    duration == 120 ~ "120 days",
    duration == 150 ~ "150 days",
    duration == 180 ~ "180 days"
  )) |>
  mutate(date = yday(spawn_date)) |> 
  filter(period == "before") |> 
  ggplot(aes(x = avg_temp, y = date, color = period)) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~as.factor(duration), ncol = 1) +
  geom_point(size = 0.5, alpha = 0.25) + 
  labs(x = "Average Temperature (°C)", 
       y = "Putative Spawn Day of Year (DOY)",
       title = "Average Temperature at Redd Locations") +
  theme_classic()


# spawn dates ~ temperature before and after
out  |>
  mutate(date = yday(spawn_date)) |> 
  filter(period == "after" & duration == 150) |>
  mutate(avg_round = round(avg_temp, digits = 0)) |> 
  ggplot() +
  geom_density_ridges(aes(x = date, y =as.factor(avg_round))) +
  theme_classic()



out_wide <- out |> 
  as_tibble() |> 
  pivot_wider(names_from = period, values_from = avg_temp) 

out_wide |> 
  filter(duration %in% c(60,90,120)) |> 
  ggplot(aes(x = before, y = after)) +
  facet_wrap(~as.factor(duration), ncol = 1) +
  geom_point(size =0.1) +
  theme_classic()

out_wide_90 <-  out_wide |> filter(duration == 90)

cor(out_wide_90$before, out_wide_90$after)

spawn_data <- read_csv(here::here("./data/russ_spawn/mfsr_spawn_cleaned.csv"))

mfsr_spawn_temps <- out |> 
  mutate(duration = case_when(
    duration == 30 ~ "30 days",
    duration == 60 ~ "60 days",
    duration == 90 ~ "90 days", 
    duration == 120 ~ "120 days",
    duration == 150 ~ "150 days",
    duration == 180 ~ "180 days"
  )) |>
  mutate(date = yday(spawn_date)) |> 
  left_join(y = spawn_data, by = join_by(redd_id == UNIQUE_ID)) |> 
  filter(period != "span" & duration != "30 days") |> 
  pivot_wider(names_from = c(duration, period), values_from = avg_temp)

glimpse(mfsr_spawn_temps)

temp_mod <-lm(yday ~ stream   *      
                       # `60 days_before` +
                       # `60 days_after`  +
                       # `90 days_before` +
                       # `90 days_after`  +
                       # `120 days_before` +
                       # `120 days_after`  +
                       # `150 days_before` ,
                       # `150 days_after`  +
                      #  `180 days_before` +
                       # `180 days_after`,
              data = mfsr_spawn_temps )

sjPlot::tab_model(temp_mod)

emmeans::emmeans(temp_mod, ~stream *  `180 days_after`)
