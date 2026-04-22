# 

# libraries
library(fs)
library(purrr)
library(readxl)
library(dplyr)

# list of spawn data from russ thurow
spawn_files <- dir_ls("./data/russ_spawn/raw/")

# load and combine data into single object
mfsr_spawn <- 
  spawn_files[c(1:3, 5)] |> # leave out 2004 (see below)
  map_df(
    read_xls, 
    .id = "file", 
    sheet = "AllRch", 
    range = cell_cols(c(1:9))  # keep cols 1-9
    ) 
  select(-"file")

# bring in 2004 data separately because easting reads as character
data_2004 <- read_xls(
  "./data/russ_spawn/raw/04_ReddMonitor_AvgXY.xls", 
  range = cell_cols(c(1:9)), 
  sheet = "AllRch"
  )
# change easting to a numeroc
data_2004$EASTING <- as.numeric(data_2004$EASTING) 

# put all data into mfsr_spawn object
mfsr_spawn <- bind_rows(mfsr_spawn, data_2004)
mfsr_spawn$UNIQUE_ID <- c(1000:(999 + nrow(mfsr_spawn)))

# write_rds(mfsr_spawn, "./data/russ_spawn/mfsr_spawn_combined.rds")
# write_csv(mfsr_spawn, "./data/russ_spawn/mfsr_spawn_combined.csv")
