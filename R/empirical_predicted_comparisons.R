
# Header ------------------------------------------------------------------

library(tidyverse)


# MFSR Empirical ----------------------------------------------------------

siegel_temp <- readRDS("./data/siegel_temperature/siegel_mfsr_comid.RDS")

empirical_temp <- readxl::read_xlsx("./data/mfsr_empirical_temps.xlsx", sheet = 2)


## Merge temp datasets -----------------------------------------------------

#make tim.date into a date for merging
siegel_temp <- siegel_temp |> 
  mutate(SampleDate = as_date(tim.date, tz = "UTC"))

#left_join to empirical and drop NAs
all_temps <- empirical_temp |>
  left_join(siegel_temp, by = join_by(COMID, SampleDate)) |>
  drop_na(prd.stream_temp)

#plot
all_temps |> 
  mutate(month = as.factor(month(SampleDate))) |> 
  ggplot(aes(x= DAILY_AVG_TEMP_C , y = prd.stream_temp, color = month)) +
  geom_point(size = 0.15) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(~month + StreamName) +
  labs(x = "Empirical Temp", y ="Predicted Temp") +
  theme_bw()

# all temps as one 
all_temps |> 
  ggplot(aes(x= DAILY_AVG_TEMP_C , y = prd.stream_temp)) +
  geom_point(size = 0.15) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Empirical Temp", y ="Predicted Temp") +
  theme_bw()



## Model fit ---------------------------------------------------------------
all_temps_mod <- all_temps |> mutate(month = as.factor(month(SampleDate)))
mod<- lm(DAILY_AVG_TEMP_C ~ prd.stream_temp , data = all_temps_mod)

sjPlot::tab_model(mod)


# Isaak 2018 Emprical -----------------------------------------------------

isaak_siegel <- readRDS("./data/siegel_temperature/Isaak_Siegel_Compare.rds")
isaak_siegel <- isaak_siegel |> 
  drop_na(DailyMean, Stream_Temp)

is_mod <- lm(DailyMean~Stream_Temp, data = isaak_siegel)
sjPlot::tab_model(is_mod)

isaak_siegel |> 
  mutate(date = mdy(SampleDate),
         month = as.factor(month(date))) |> 
  ggplot(aes(x= DailyMean , y = Stream_Temp, color = month)) +
  #geom_point(size = 0.01) +
  geom_smooth(method = "lm") +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(~month) +
  labs(x = "Empirical Temp", y ="Predicted Temp") +
  theme_bw()

is_summer <- isaak_siegel |> 
  mutate(date = mdy(SampleDate),
         month = month(date)) |>
  filter(month %in% c(6:8))

is_sum_mod <- lm(DailyMean~Stream_Temp, data = is_summer)
sjPlot::tab_model(is_sum_mod)



# compare russ reach data with gwynne + siegel data --------------------------------

### get comids for sulphur 2 --------------------------------
reach_comids <- read_csv(here::here("./data/russ_spawn/comids_in_reaches.csv"))

sulphur2_comids <- reach_comids |> filter(REACH == "SULPHUR2") |> pull(COMID)

### summarize reach temps --------------------------------

reach_temps <- read_csv(here::here('./data/russ_spawn/raw/russ_reach_temps_02.csv'))

reach_temps_sum <- reach_temps |> 
  mutate(date_dttm = mdy_hm(Date),
         date = date(date_dttm)) |> 
  group_by(Stream, Reach, date) |> 
  summarise(mean_temp = mean(Temperature))

### filter to sulphur2
sulphur2_reach_temps <- reach_temps_sum |> 
  filter(Stream == "Sulphur" & Reach == 2)

### gwynne temps sulphur2 2002--------------------------------

sulphur2_gwynne <- empirical_temp |> 
  filter(COMID %in% sulphur2_comids & 
           SampleDate >= min(sulphur2_reach_temps$date) & 
           SampleDate <= max(sulphur2_reach_temps$date))

#no matches!

### siegel temps sulphur2 2002 --------------------------------

sulphur2_siegel <- siegel_temp |> 
  filter(COMID %in% sulphur2_comids[6] & 
           SampleDate >= min(sulphur2_reach_temps$date) & 
           SampleDate <= max(sulphur2_reach_temps$date)) 

sulphur2_mod <- lm(sulphur2_reach_temps$mean_temp ~ sulphur2_siegel$prd.stream_temp)
summary(sulphur2_mod)

