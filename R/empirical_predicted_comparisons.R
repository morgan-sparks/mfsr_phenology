
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



## Model fit ---------------------------------------------------------------

mod<- lm(DAILY_AVG_TEMP_C ~ prd.stream_temp, data = all_temps)

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
  filter(month %in% c(6:9))

is_sum_mod <- lm(DailyMean~Stream_Temp, data = is_summer)
sjPlot::tab_model(is_sum_mod)
