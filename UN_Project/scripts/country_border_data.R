# Joins the border + information dataset with UN membership status. 

# Author: JB Lim
# Version: 2020-02-24

# Libraries
library(tidyverse)

# Parameters
# file names
data_file <- here::here("c01-own/data/country_data.rds")
border_file <- here::here("c01-own/data/country_border.rds")


path_file <- here::here("c01-own/data/country_border_data.rds")
#===============================================================================
data <-
  data_file %>%
  read_rds() %>%
  drop_na(un_member_date)

border_file %>%
  read_rds() %>%
  mutate_at(vars(iso_a3), str_to_lower) %>%
  mutate(
    iso_a3 = 
      if_else(sovereignt == "France" & continent == "Europe", "fra", iso_a3),
    iso_a3 =
      if_else(sovereignt == "Norway" & continent == "Europe", "nor", iso_a3)
  ) %>%
  left_join(
    data %>%
      select(geography, un_member_date),
    by = c("iso_a3" = "geography")
  ) %>%
  drop_na(un_member_date) %>%
  write_rds(path_file)

