usethis::use_course("bit.ly/wood-survey-data",
                    destdir = "data-raw")
3

# Paths

raw_data_path <- here::here("data-raw", 
                            "wood-survey-data-master")

here::here(raw_data_path, "individual")

fs::dir_ls(here::here(raw_data_path, "individual"))

individual_paths <- fs::dir_ls(
  here::here(raw_data_path,
             "individual"))

length(individual_paths)

indiv_df <- readr::read_csv(individual_paths[1])

View(indiv_df)

skimr::skim(indiv_df)

# Iteration
for (i in 1:10) {
  print(paste0("i is ", i))
}

indiv_df_list <- vector(mode = "list",
                        length = length(individual_paths))

for (i in seq_along(individual_paths)) {
indiv_df_list[[i]] <- readr::read_csv(individual_paths[i],
                  show_col_types = FALSE)
}

# Using objects and names instead of indices
indiv_df_list <- vector(mode = "list",
                        length = length(individual_paths))
names(indiv_df_list) <- basename(individual_paths)

for (path in individual_paths) {
indiv_df_list[[basename(path)]] <- readr::read_csv(path, show_col_types = FALSE)
}

rbind(indiv_df_list)

#Combine all dfs into one 
library(dplyr)
do.call(what = "rbind", args = indiv_df_list)
purrr::reduce(indiv_df_list, .f = rbind)

# Functional iteration ----
individual <- purrr::map(.x = individual_paths,
                         ~ readr::read_csv(
                           .x,
                           col_types = readr::cols(.default = "c"),
                           show_col_types = FALSE)
) %>% 
  purrr::list_rbind() %>%
  readr::type_convert()

# JOINS ----
library(dplyr)
band_members

band_instruments

#inner join - only keeps rows when data exists in both parts of the join
band_members %>% 
  inner_join(band_instruments)

#left join - joins what it can from one side

band_members %>%
  left_join(band_instruments)

#right join - joins from the other side

#full join

band_members %>% 
  full_join(band_instruments)

#antijoin - just returns only the data that isnt matched across the parts of the join

## join map and plot info

maptag <- readr::read_csv(
  fs::path(raw_data_path, "vst_mappingandtagging.csv")
)

names(maptag)
names(individual)

names(individual)[names(individual) %in% names(maptag)]

individual %>% 
  left_join(maptag)

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

# Functions ----
add <- function(x, y) {
  x + y
  return(NULL)
}

x <- 4
y <- 2

add(x, y)

usethis::use_r("geolocate")


# Dataspice ----
library(dataspice)
library(dplyr)

create_spice()
individual <- readr::read_csv(
  here::here("data", "individual.csv")
)
range(individual$date)
range(individual$decimal_latitude)
range(individual$decimal_longitude)

"NEON Domain areas D01:09"

prep_attributes()



variables <- readr::read_csv(
  here::here(
    "data-raw",
    "wood-survey-data-master",
    "NEON_vst_variables.csv"
  )
)


variables$fieldName
attributes <- readr::read_csv(
  here::here(
    "data",
    "metadata",
    "attributes.csv"
  )
)
attributes$variableName

variables <- readr::read_csv(
  here::here(
    "data-raw",
    "wood-survey-data-master",
    "NEON_vst_variables.csv"
  )
) %>% 
  dplyr::mutate(
    fieldName = janitor::make_clean_names(fieldName)
  ) %>%
  dplyr::select(fieldName, description, units)

attributes <- readr::read_csv(
  here::here(
    "data",
    "metadata",
    "attributes.csv"
  )
) %>% 
  dplyr::select(-description, -unitText)

dplyr::left_join(attributes,
                 variables,
                 by = c(variableName = "fieldName")) %>% 
  dplyr::rename(unitText = "units") %>%
  readr::write_csv(
    file = here::here(
      "data", "metadata","attributes.csv"
    )
  )

# creat json-ld file
write_spice()
build_site(out_path = "data/index.html")

## GGplot

library(ggplot2)

individual %>% 
  ggplot(aes(x = log(height))) + 
  geom_density(fill = "grey", colour = "grey")

#barplot

analysis_df %>% 
    ggplot(aes(y = growth_form, 
             fill = growth_form, colour = growth_form)) + geom_bar(alpha = 0.5, show.legend = FALSE)

# Plotting 
analysis_df %>%
  ggplot(aes(x = log(stem_diameter),
             colour = growth_form,
             fill = growth_form))+ 
  geom_density(alpha = 0.5) + 
  facet_wrap(~growth_form)


analysis_df %>%
  tidyr::pivot_longer(
    cols = c(stem_diameter, height),
    names_to = "var",
    values_to = "value"
  ) %>%   
  ggplot(aes(x = log(value),
             y = growth_form,
             colour = growth_form, 
             fill = growth_form)) +
  geom_violin(alpha = 0.5, trim = TRUE, show.legend = FALSE) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  facet_grid(~var)

