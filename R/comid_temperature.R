library(dplyr); library(lubridate)


comid_temperature <- function(temperature_data, 
                              comid, 
                              redd_id, 
                              days, 
                              date, 
                              period = c("before, after, span")){
  
  #make data a dttm
  dttm = mdy(date)
  
  # make a seq of dates depending on period you want
  if(period == "after"){
    #days after
    duration <- seq(dttm, dttm + (days -1), by = "days" )} else if (period == "before"){
      #days before
      duration <- seq(dttm - (days-1), dttm, by = "days" )} else if (period == "span"){
        # days span
        duration <-  seq (dttm -ceiling(days/2), dttm + floor(days/2), by = "days")}
  
  # calculate average temp
  avg_temp <- data |> 
    filter(COMID == comid) |>
    mutate(spawn_date = ymd(tim.date)) |> 
    filter(spawn_date %in% duration ) |> 
    summarise(avg_temp = mean(prd.stream_temp))
  
  # out object with all the goodies
  out <- tibble(spawn_date = mdy(date),
                comid = comid,
                redd_id = redd_id,
                period = period,
                duration = days,
                avg_temp = avg_temp$avg_temp)
  return(out)
}



# Example -----------------------------------------------------------------

# temperature_data = readRDS(here::here("./data/siegel_temperature/siegel_mfsr_comid.RDS"))
# 
# comid = 23519529
# 
# redd_id = 1
# 
# date = "08312001"
# 
# days = 90 
# 
# period = "before"
# 
# comid_temperature(temperature_data = temperature_data,
#            comid = comid,
#            redd_id = redd_id,
#           days = days,
#           date = date,
#           period = "before")
