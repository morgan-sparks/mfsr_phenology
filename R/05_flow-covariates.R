library(dataRetrieval); library(tidyverse)


# Import flow data for MFSR gage at Middle Fork Lodge ---------------------

siteNumber <- "13309220"
parameterCd <- "00060" # Discharge
startDate <- "2001-01-01"
endDate <- "2005-12-31"

mfsr_discharge <- readNWISdv(siteNumber, parameterCd, startDate, endDate)
mfsr_discharge <- renameNWISColumns(mfsr_discharge)

# write out flow data
write_csv(mfsr_discharge, here::here("./data/mfsr_flow.csv"))

# quick plot
mfsr_discharge |> 
  ggplot(aes(x = Date, y =Flow)) +
  geom_point() +
  geom_line() +
  theme_classic()


# Function for summarizing flow -------------------------------------------

discharge_interval <-  function(spawn_date){
  flow_data <- read_csv(here::here("./data/mfsr_flow.csv"))
  days_30 <- seq(spawn_date -29, spawn_date, by = 'days')
  days_60 <- seq(spawn_date -59, spawn_date, by = 'days')
  days_90 <- seq(spawn_date -89, spawn_date, by = 'days')
  
  flow_30 <- flow_data |> 
    filter(Date %in% days_30) |> 
    summarise(mean_flow = mean(Flow))
  
  flow_60 <- flow_data |> 
    filter(Date %in% days_60) |> 
    summarise(mean_flow = mean(Flow))
  
  flow_90 <- flow_data |> 
    filter(Date %in% days_90) |> 
    summarise(mean_flow = mean(Flow))
    
  out<- tibble( spawn_date = spawn_date,
                flow_30 = flow_30$mean_flow,
                flow_60 = flow_60$mean_flow,
                flow_90 = flow_90$mean_flow)
  
  return(out)
}


# Calculate flow metrics for each spawn date ------------------------------

spawn_data <- read_csv(here::here("./data/russ_spawn/mfsr_spawn_cleaned.csv"))

spawn_dates <- ymd(unique(spawn_data$date))

spawn_flows <- spawn_dates |> 
purrr::map_df(discharge_interval)

write_csv(spawn_flows, here::here("./data/spawn_flows.csv"))
  