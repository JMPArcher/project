## code to prepare `individual` dataset goes here
## Setup ---- 
library(dplyr)
source(here::here("R", "geolocate.R"))

## Combine individual tables ----
## Create Paths to inputs
raw_data_path <- here::here("data-raw", 
                            "wood-survey-data-master")

individual_paths <- fs::dir_ls(
  here::here(raw_data_path, "individual")
 )
## read in all individual tables into one

individual <- purrr::map(.x = individual_paths,
                         ~ readr::read_csv(
                           .x,
                           col_types = readr::cols(.default = "c"),
                           show_col_types = FALSE)
) %>% 
  purrr::list_rbind() %>%
  readr::type_convert()


individual %>% 
  readr::write_csv(
    file = fs::path(raw_data_path, "vst_individuals.csv")
  )

##Combine NEON data tables ----
##read in additional tables
maptag <- readr::read_csv(
  fs::path(
    raw_data_path,
    "vst_mappingandtagging.csv"
  ),
  show_col_types = FALSE
) %>%
  select(-eventID)

perplot <- readr::read_csv(
  fs::path(
    raw_data_path,
    "vst_perplotperyear.csv"
  ),
  show_col_types = FALSE
) %>%
  select(-eventID)

# Left join tables to individual
individual %<>%
  left_join(maptag,
            by = "individualID",
            suffix = c("", "_map")
  ) %>%
  left_join(perplot,
            by = "plotID",
            suffix = c("", "_ppl")
  ) %>%
  assertr::assert(
    assertr::not_na, stemDistance, stemAzimuth, pointID,
    decimalLongitude, decimalLatitude, plotID
  )

# Gelocate indiviudals ----
individual <- individual %>% mutate(
  stemLat = get_stem_location(
    decimalLongitude, decimalLatitude,
    stemAzimuth, stemDistance
  )$lat,
  stemLon = get_stem_location(
    decimalLongitude, decimalLatitude,
    stemAzimuth, stemDistance
  )$lon,
) %>% 
  janitor::clean_names()

fs::dir_create("data")

individual %>% 
  readr::write_csv(
    here::here("data", "individual.csv")
  )



