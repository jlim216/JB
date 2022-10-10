# Reads in and writes out country data from gapminder.

# Author: JB Lim
# Version: 2020-02-06

# Libraries
library(tidyverse)
library(readxl)

# Parameters
# File Name
file_name <- "/Users/jblim/Downloads/Data Geographies - v1 - by Gapminder.xlsx"
path_name <- here::here("c01-own/data/country_data.rds")

# rename types
vars_types <-
  cols_only(
    geo = col_character(),
    name = col_character(),
    four_regions = col_character(),
    'UN member since' = col_date(),
    'World bank region' = col_character(),
    'World bank, 4 income groups 2017' = col_character()
  )

# rename variables
vars_rename <-
  c(
    "geography" = "geo",
    "country_name" = "name",
    "continent" = "four_regions",
    "un_member_date" = "UN member since",
    "subregion" = "World bank region",
    "income_level" = "World bank, 4 income groups 2017"
  )


#===============================================================================
file_name %>%
  read_excel(
    sheet = 2,
    col_names = TRUE
  ) %>%
  rename(!!! vars_rename) %>%
  write_rds(path_name)


