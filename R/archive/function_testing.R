library(purrr)
spawn_data <- read_csv(here::here("./data/russ_spawn/mfsr_spawn_cleaned.csv"))

data <- spawn_data |> 
  select(comid = COMID, redd_id = UNIQUE_ID, date =DATE)





test <- map(comid_temperature,
             temperature_data = siegel_mfsr_comid,
             date = data$date,
            redd_id = data$redd_id,
            comid = data$comid,
             days = 90,
            period = "before")

function(temperature_data, 
         comid, 
         redd_id, 
         days, 
         date, 
         period = c("before, after, span"))