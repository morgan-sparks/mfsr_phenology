library(tidyverse); library(ggridges)

# now using Dona's COMID linked spawn data set
mfsr_spawn <- read_csv(here::here("./data/russ_spawn/raw/MFSR_n4584_SnapWithCOMID_raw.csv"))


#style not, all UPPER CASE are raw columns, lower case are appended columns

# cleaned spawn data
mfsr_spawn_cleaned <- mfsr_spawn |> 
  #drop_na(REACH) |>  #remove empty reaches
 # mutate(across(DATE, as.character)) |>  #make dates into character
 # mutate(date = mdy(gsub('.*^','0',DATE))) |> # add 0 to dates so we can use lubridate
  mutate(date = mdy(DATE)) |> 
  mutate(year = year(date)) |> # year column
  mutate(yday = yday(date)) |> # DOY column
  mutate(month = month(date)) |> # date column
  filter(OBJ_CLASS %in% c("Redd", "REDD")) |> # filter only for redds
  filter(UNIQUE_ID != 3589)

# plot to look at densities
mfsr_spawn_cleaned |> 
  ggplot(aes(x = yday, y = as.character(year))) +
  geom_density_ridges(scale = 1.1) +
  labs(x = "Day of Year", y = "Year") +
  theme_classic()

# tally by year
mfsr_spawn_cleaned |> 
  group_by(year) |> 
  tally()

reach_names <- unique(mfsr_spawn_cleaned$REACH)

mfsr_spawn_cleaned <- mfsr_spawn_cleaned |> 
  mutate(stream = case_when(
    REACH %in% c("BEAVER") ~ "Beaver",
    REACH %in% c("BIG1", "BIG2", "BIG3", "BIG4", "BIG5", "BIG6", "BIG7") ~ "Big",
    REACH %in% c("MARSH1", "Marsh2") ~ "Marsh",
    REACH %in% c("BEARVAL1", "BEARVAL2") ~ "Bear Valley",
    REACH %in% c("CAMAS1", "CAMAS2", "CAMAS3") ~ "Camas",
    REACH %in% c("ELK2", "ELK3") ~ "Elk",
    REACH %in% c("Cape Horn", "CAPE HORN", "Cape Horn Cr.") ~ "Cape Horn",
    REACH %in% c("Knapp") ~ "Knapp",
    REACH %in% c("LOON1", "LOON2", "LOON3") ~ "Loon",
    REACH %in% c("SULPHUR2") ~ 'Sulphur'))

write_csv(mfsr_spawn_cleaned, "./data/russ_spawn/mfsr_spawn_cleaned.csv")



