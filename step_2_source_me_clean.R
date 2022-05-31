# Copyright 2022 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

# This script cleans the MPI data.

# Data quality problems:
# 1) Categorical variables project_name, construction_type, construction_subtype, project_type, region, project_category_name
# should be constant for a given project_id.
# 2) In the remaining variables: estimated_cost, project_status, first_entry_date, telephone there are missing values, both:
# 2a) explicit (the presence of an absence) i.e. variables recorded as NA and
# 2b) implicit. (the absence of a presence) i.e. entire quarter of data missing for project reported previously and subsequently.

# libraries--------
if (!"tidyverse" %in% names(sessionInfo()$otherPkgs)) library(tidyverse)
if (!"lubridate" %in% names(sessionInfo()$otherPkgs)) library(lubridate) # years and months not exported objects
source(here::here("R","functions.R"))
mpi_raw <- readRDS(here::here("processed_data", "mpi_raw.rds"))
global_start_date <- min(mpi_raw$last_update, na.rm = TRUE) #the date of the first MPI file used.
global_last_date <- max(mpi_raw$last_update, na.rm = TRUE) #the date of the last MPI file used.

nested <- mpi_raw %>%
  group_by(project_id) %>%
  nest() %>%
  mutate(data = map(data, mode_fill)) %>% # replace all the categorical variables that SHOULD be constant with their modal value
  unnest(data) %>%
  group_by( # grouping by all these (redundant) constants before nesting leaves only true VARI-ables in nested data.
    project_id,
    project_name,
    construction_type,
    construction_subtype,
    project_type,
    region,
    project_category_name
  ) %>%
  nest() %>%
  # convert implicit missing to explicit and fills up (backwards in time) then down (forward in time).------
  mutate(data = map(data, updown_fill))

mpi_clean <- nested %>%
  unnest(data) %>%
  filter(last_update >= global_start_date)%>% #trim off data from before the date of the first file.
  nest()%>%
  mutate(data=map(data, add_weight))%>%
  #without weights group averages would be biased towards long lived projects i.e. with weights each project gets equal weight regardless of how long lived.
  unnest(data)

# Fabricated data is for projects that have NEVER reported an estimated cost.  These NAs are replaced by the average of
# 1) 15M: which is the minimum project size to be considered "major" and
# 2) the average estimated cost among projects that share the same
#     - construction_type,
#     - construction_subtype,
#     - project_type and
#     - project_category_name
mpi_fabricated<- mpi_clean%>%
  group_by(construction_type, construction_subtype, project_type, project_category_name)%>%
  mutate(estimated_cost = ifelse(is.na(estimated_cost), (weighted.mean(estimated_cost, w = weight, na.rm = TRUE)+15)/2, estimated_cost))

saveRDS(mpi_clean, here::here("processed_data","mpi_clean.rds"))
saveRDS(mpi_fabricated, here::here("processed_data","mpi_fabricated.rds"))
# Projects that were in the MPI and then were not, where they were never reported complete... possibly abandoned?------
whats_the_deal <- mpi_clean%>%
  group_by(
    project_id,
    project_name,
    construction_type,
    construction_subtype,
    project_type,
    region,
    project_category_name
  ) %>%
  nest() %>%
  mutate(possibly_abandoned = map_lgl(data, possibly_abandoned)) %>%
  filter(possibly_abandoned == TRUE) %>%
  mutate(
    last_project_status = map_chr(data, last_var, project_status),
    last_update = map_chr(data, last_var, last_update),
    last_phone = map_chr(data, last_var, telephone)
  ) %>%
  ungroup() %>%
  select(project_name, last_project_status, last_update, last_phone)
saveRDS(whats_the_deal, here::here("processed_data","whats_the_deal.rds"))

# do the same for Man's long file------------

mpi_raw_long <- readRDS(here::here("processed_data", "mpi_raw_long.rds"))

nested_long <- mpi_raw_long %>%
  group_by(project_id) %>%
  nest() %>%
  mutate(data = map(data, mode_fill_long))%>% # replace all the categorical variables that SHOULD be constant with their modal value-----
unnest(data) %>%
  group_by( # grouping by all these (redundant) constants before nesting leaves only true variables in nested data.
    project_id,
    project_name,
    project_type,
    region,
    project_category_name
  ) %>%
  nest() %>% # converts implicit missing to explicit and fills up (backwards in time) then down (forward in time).------
mutate(data = map(data, updown_fill_long))

mpi_clean_long <- nested_long %>%
  mutate(data=map(data, add_weight))%>%
  #without weights group averages would be biased towards long lived projects i.e. with weights each project gets equal weight regardless of how long lived.
  unnest(data)

# Fabricated data is for projects that have NEVER reported an estimated cost.  These NAs are replaced by the average of-----
# 1) 15M: which is the minimum project size to be considered "major" and
# 2) the average estimated cost among projects that share the same
#     - project_type and
#     - region

mpi_fabricated_long<- mpi_clean_long%>%
  group_by(project_type, region)%>%
  mutate(estimated_cost = ifelse(is.na(estimated_cost), (weighted.mean(estimated_cost, w = weight, na.rm = TRUE)+15)/2, estimated_cost))

saveRDS(mpi_clean_long, here::here("processed_data","mpi_clean_long.rds"))
saveRDS(mpi_fabricated_long, here::here("processed_data","mpi_fabricated_long.rds"))
