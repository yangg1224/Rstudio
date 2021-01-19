#### Workspace set-up ####
# Libraries
library(opendatatoronto)
library(tidyverse)
library(lubridate)

# Get the data
# Based on https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-12-01/readme.md
all_data <- 
  opendatatoronto::search_packages("Daily Shelter Occupancy") %>% 
  opendatatoronto::list_package_resources() %>% 
  dplyr::filter(name %in% c("daily-shelter-occupancy-2017-csv",
                            "daily-shelter-occupancy-2018-csv", 
                            "daily-shelter-occupancy-2019-csv", 
                            "daily-shelter-occupancy-2020.csv")) %>% 
  group_split(name) %>% # Don't totally get this
  map_dfr(get_resource, .id = "file")


#### Data cleaning and prep ####
# The main issue with the data is the dates. In 2017-2019 (inc) they are ymd, but
# for 2020 they are mdy. The separator is also inconsistent. The SUPER weird 
# thing is that they're not in order in the raw data, but I checked by 
# splitting them into pieces (day, month, year) and the counts seem okay; weird though.
toronto_shelters <-
  all_data %>% 
  janitor::clean_names() %>% # Make the column names easier to type
  mutate(occupancy_date = str_remove(occupancy_date, "T[:digit:]{2}:[:digit:]{2}:[:digit:]{2}"),
         occupancy_date = str_replace_all(occupancy_date, "/", "-")
  ) %>% # 1st line removes times (probs don't actually need to do) and 2nd makes the separation consistent
  mutate(date = case_when(
    file == "4" ~ mdy(occupancy_date, quiet = TRUE), 
    file %in% c("1", "2", "3") ~ ymd(occupancy_date, quiet = TRUE),
    TRUE ~ NA_Date_
  )
  ) %>% # The parsing is different depending on whether it's 2017-2019 or 2020. Last line is a catch-all - shouldn't get there.
  select(date, organization_name:capacity) %>% 
  rename(occupancy_date = date)

#### Analysis ####
# Interested in availability of shelter spots in Toronto on the basis of sector.
# Different sectors focus on different folks: Co-ed, Families, Men, Women, Youth.
# So for each day for each sector we have a proportion (note: horrifyingly >1 is possible).
# Based on: https://github.com/llendway/tidy_tuesday_in_thirty/blob/main/2020_12_01_tidy_tuesday.Rmd
usage_rate <- 
  toronto_shelters %>% 
  tidyr::drop_na(occupancy, capacity) %>% # We only want rows that have data for both occupancy and capacity
  group_by(occupancy_date, sector) %>% # We want to know the occupancy by date and sector
  summarise(the_sum = sum(occupancy),
            the_capacity = sum(capacity),
            the_usage = the_sum / the_capacity, .groups = 'drop')

library(ggplot2)





