# Reads in and writes out UN General Assembly Voting data.

# Author: JB Lim
# Version: 2020-02-06

# Libraries
library(tidyverse)

# Parameters
  #File Name
un_votes_file <- "/Users/jblim/Stanford University/ENGR 150/UNdata.csv"
path_name <- here::here("c01-own/data/un_voting.rds")
#===============================================================================
un_votes_file %>%
  read_csv(
    col_names = TRUE,
    col_types = cols_only(
      rcid = col_integer(),
      ccode = col_integer(),
      member = col_integer(),
      vote = col_integer(),
      Country = col_character(),
      Countryname = col_character(),
      year = col_integer(),
      session = col_integer(),
      abstain = col_integer(),
      yes = col_integer(),
      no = col_integer(),
      importantvote = col_logical(),
      date = col_date(),
      unres = col_character(),
      descr = col_character(),
      me = col_logical(),
      nu = col_logical(),
      di = col_logical(),
      hr = col_logical(),
      co = col_logical(),
      ec = col_logical()
    )
  ) %>%
  rename_all(str_to_lower) %>%
  write_rds(path_name)
