# Description

# Author: JB Lim
# Version: 2020-02-24

# Libraries
library(tidyverse)
library(sf)

# Parameters
file_name <-
  "/Users/jblim/Downloads/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp"
path_name <- here::here("c01-own/data/country_border.rds")

#===============================================================================
file_name %>%
  read_sf() %>% 
  rename_all(str_to_lower) %>%
  select(
    sovereignt, 
    pop_est, 
    pop_rank,
    gdp_md_est,
    economy,
    income_grp,
    iso_a3,
    continent,
    subregion,
    region_wb,
    geometry
  ) %>%
  write_rds(path_name)
