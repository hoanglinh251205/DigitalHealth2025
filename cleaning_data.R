# PACKAGES ----
install.packages(c("pacman", "here", "janitor", "tidyverse", "rio"))

pacman::p_load(
  rio,          # data import/export  
  here,         # file management  
  janitor,      # data cleaning and tables
  tidyverse     # data management and visualization
)

# IMPORT RAW DATA ----

linelist_raw <- import("tfr.CSV")

# CLEANING DATA ----
linelist <- linelist_raw %>% 
  
  # standardize column name syntax
  janitor::clean_names() %>%
  
  # select columns
  select(location_name, year_id, scenario_name, val, upper, lower) %>%
  
  # de-duplicate
  distinct() %>%
  
  # convert column classes # no need because they are already correct
  # linelist$location_name (character)
  # linelist$year_id (integer)
  # linelist$scenario_name (character)
  # linelist$val (numeric)
  
  # re-code values # no need because they are already correct

  # filter to reference scenario
  filter(scenario_name == "Reference") 
