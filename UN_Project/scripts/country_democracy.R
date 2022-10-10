# Reads in and writes out democracy data

# Author: JB Lim
# Version: 2020-02-06

# Libraries
library(tidyverse)

# Parameters
  #File Name
democracy_file <- "/Users/jblim/Stanford University/ENGR 150/electoral democracy.csv"
path_name <- here::here("c01-own/data/country_democracy.rds")
#===============================================================================
democracy_file %>%
  read_csv(
    skip = 2,
    col_name = TRUE,
    col_types = cols_only(
      Country = col_character(),
      Electoral_Democracy  = col_character()
    )
  ) %>%
  rename_all(str_to_lower) %>%
  write_rds(path_name)
