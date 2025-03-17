# script to take full download from Siegel et al. 2023 and 
# grab middle fork salmon river sites

#source data: https://zenodo.org/records/8174951

#libraries
library(fs); library(stringr); library(purrr); library(readr)

# file list of all files for salmon huc6 download
huc_17062_files <- dir_ls("~/Downloads/st_pred_170602/")

# put mfsr files in a df together
mfsr_huc8 <- huc_17062_files |> 
  str_subset(pattern = ("17060205|17060206")) |> # str subset on mfsr huc8
  map_df(read_csv, .id = "file", col_types = cols())

write_rds(mfsr_huc8, "./data/siegel_temperature/siegel_mfsr.rds")
