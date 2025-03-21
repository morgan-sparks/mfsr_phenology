library(tidyverse)

comid_temperature <- function(temperature_data,
                              comid, # redd dataset
                              redd_id, # redd dataset
                              spawn_date, # redd dataset
                              day_spread,
                              period = c("before, after, span")) {
  # make data a dttm
  dttm <- mdy(spawn_date)
  
  # make a seq of dates depending on period you want
  if (period == "after") {
    # days after
    duration <- seq(dttm, dttm + (day_spread - 1), by = "days")
  } else if (period == "before") {
    # days before
    duration <- seq(dttm - (day_spread - 1), dttm, by = "days")
  } else if (period == "span") {
    # days span
    duration <- seq(dttm - ceiling(day_spread / 2), dttm + floor(day_spread / 2), by = "days")
  }
  
  # calculate average temp
  avg_temp <- temperature_data |>
    filter(COMID == comid) |>
    filter(tim.date %in% duration) |>
    summarise(avg_temp = mean(prd.stream_temp))
  
  # out object with all the goodies
  out <- tibble(
    spawn_date = mdy(spawn_date),
    comid = comid,
    redd_id = redd_id,
    period = period,
    duration = day_spread,
    avg_temp = avg_temp$avg_temp
  )
  return(out)
}



# Example -----------------------------------------------------------------

# temperature_data = readRDS(here::here("./data/siegel_temperature/siegel_mfsr_comid.RDS"))
# 
# comid = 23519529
# 
# redd_id = 1
# 
# spawn_date = "08312001"
# 
# day_spread = 90
# 
# period = "before"
# 
# comid_temperature(temperature_data = temperature_data,
#                   comid = comid,
#                   redd_id = redd_id,
#                   day_spread = day_spread,
#                   spawn_date = spawn_date,
#                   period = period)
