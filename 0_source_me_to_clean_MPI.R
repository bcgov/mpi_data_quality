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

# This script downloads previous MPI data from https://www2.gov.bc.ca/gov/content/employment-business/economic-development/industry/bc-major-projects-inventory/recent-reports

#' Before running:
#' (re)place the file latest.xlsx in the directory raw_data.

# libraries--------
library(tidyverse)
library(lubridate)
#functions-------------------
source(here::here("R","functions.R"))
#the script---------------------
if (!file.exists(here::here("raw_data"))) dir.create(here::here("raw_data"))
if (!file.exists(here::here("processed_data"))) dir.create(here::here("processed_data"))
mpi_url_to_scrape <- "https://www2.gov.bc.ca/gov/content/employment-business/economic-development/industry/bc-major-projects-inventory/recent-reports"
mpi_scraped <- rvest::read_html(mpi_url_to_scrape)
mpi_links <- rvest::html_attr(rvest::html_nodes(mpi_scraped, "a"), "href") # all the links
mpi_links <- mpi_links[mpi_links %>% startsWith("/assets/") & mpi_links %>% endsWith(".xlsx")] %>% # stubs of the links we want.
  na.omit()
mpi_links <- paste0("https://www2.gov.bc.ca", mpi_links) # paste the head onto the stubs
mpi_files <- paste0("mpi_dl", 1:length(mpi_links), ".xlsx") # sane file naming.
# NOTE THAT YOU ONLY NEED TO DOWNLOAD THE DATA ONCE PER QUARTER... FOLLOWING LINE CAN BE COMMENTED OUT TO SKIP DOWNLOAD
mapply(download.file, mpi_links, here::here("raw_data", mpi_files), mode="wb") # downloads all the old mpi files into folder raw_data
########
mpi_all_sheets <- sapply(here::here("raw_data", mpi_files), readxl::excel_sheets) # gets all the sheets
sheet_starts_with_mpi <- lapply(mpi_all_sheets, function(x) x[startsWith(x, "mpi")]) %>%
  unlist(use.names = FALSE) # could break... assumes excel sheet naming remains consistent.
mpi_sheets <- c(sheet_starts_with_mpi) #only most recent files have same structure
mpi_files <- mpi_files[1:length(mpi_sheets)] # could break... assumes excel sheet naming remains consistent.
mpi_nested <- tibble(file = here::here("raw_data", mpi_files), sheet = mpi_sheets) %>%
  mutate(
    data = map2(file, sheet, readxl::read_excel),
    data = map(data, janitor::clean_names),
    data = map(data, fix_last_update), #replaces last update with modal value for the quarter.
    ncol = map(data, ncol)
  )

mpi_nested <- mpi_nested[!duplicated(mpi_nested$sheet),] #for some reason duplicates?

#file to clean---------
latest_data <- tibble(file=here::here("raw_data","latest.xlsx"),
                      sheet="should only be one sheet")%>%
  mutate(
    data = map(file, readxl::read_excel),
    data = map(data, janitor::clean_names),
    data = map(data, fix_last_update),
    ncol = map(data, ncol)
  )

mpi_nested <- bind_rows(mpi_nested, latest_data)

mpi_raw <- data.table::rbindlist(mpi_nested$data, use.names = FALSE) %>%
  as_tibble() %>%
  mutate(
    last_update = lubridate::round_date(last_update, unit = "quarter") - months(1), # rounding to quarter gives months 1,4,7,10... we want 12,3,6,9
    first_entry_date = lubridate::round_date(first_entry_date, unit = "quarter") - months(1),
    project_category_name = fct_collapse(project_category_name,
                                         `Residential & Commercial` = c(
                                           "Residential/Commercial",
                                           "Residential Commercial"
                                         )
    )
  )

global_start_date <- min(mpi_raw$last_update, na.rm = TRUE) # the date of the first MPI file used.
global_last_date <- max(mpi_raw$last_update, na.rm = TRUE) # the date of the last MPI file used.
#clean the data-----------------

mode_filled <- mpi_raw %>%
  group_by(project_id) %>%
  nest() %>%
  mutate(data = map(data, mode_fill)) %>% # replace all the categorical variables that SHOULD be constant with their modal value
  unnest(data)

nested <- mode_filled%>%
  group_by(project_id, #all the non variable variables
           project_name,
           project_description,
           construction_type,
           construction_subtype,
           project_type,
           region ,
           municipality,
           project_category_name,
           standardized_start_date,
           latitude,
           longitude,
           latitude_dms,
           longitude_dms
  ) %>%
  nest()%>%
  mutate(data = map(data, updown_fill))%>%
  arrange(project_id)

mpi_clean <- nested %>%
  unnest(data)%>%
  select(project_id, #make sure the columns are in correct order
         project_name,
         project_description,
         estimated_cost,
         update_activity,
         environmental_assessment_stage,
         construction_type,
         construction_subtype,
         project_type,
         region,
         municipality,
         developer,
         architect,
         project_status,
         project_stage,
         project_category_name,
         public_funding_ind,
         provinvial_funding,
         federal_funding,
         municipal_funding,
         other_public_funding,
         green_building_ind,
         green_building_desc,
         clean_energy_ind,
         indigenous_ind,
         indigenous_names,
         indigenous_agreement,
         construction_jobs,
         operating_jobs,
         standardized_start_date,
         standardized_completion_date,
         latitude,
         longitude,
         latitude_dms,
         longitude_dms,
         telephone,
         project_website,
         first_entry_date,
         last_update)%>%
  filter(last_update >= global_start_date) %>% # trim off data from before the date of the first file.
  nest() %>%
  mutate(data = map(data, add_weight)) %>%
  # without weights group averages would be biased towards long lived projects i.e. with weights each project gets equal weight regardless of how long lived.
  unnest(data)

# Fabricated data is for projects that have NEVER reported an estimated cost.  These NAs are replaced by the average of
# 1) 15M: which is the minimum project size to be considered "major" and
# 2) the average estimated cost among projects that share the same
#     - construction_type,
#     - construction_subtype,
#     - project_type and
#     - project_category_name
mpi_fabricated <- mpi_clean %>%
  group_by(construction_type, construction_subtype, project_type, project_category_name) %>%
  mutate(estimated_cost = ifelse(is.na(estimated_cost), (weighted.mean(estimated_cost, w = weight, na.rm = TRUE) + 15) / 2, estimated_cost))

saveRDS(mpi_raw, here::here("processed_data", "mpi_raw.rds"))
saveRDS(mpi_clean, here::here("processed_data", "mpi_clean.rds"))
saveRDS(mpi_fabricated, here::here("processed_data", "mpi_fabricated.rds"))

mpi_clean_current <- mpi_clean%>%
  ungroup()%>%
  filter(last_update==max(last_update))

mpi_raw_current <- mpi_raw%>%
  filter(last_update==max(last_update))%>%
  arrange(project_id)

raw_and_clean_current <- full_join(mpi_raw_current, mpi_clean_current, by="project_id", suffix = c("_raw","_clean"))%>% #add flags for changes
  mutate(project_name_change = if_else(project_name_clean == project_name_raw,
                                  FALSE,
                                  TRUE,
                                  TRUE),
         project_description_change = if_else(project_description_clean==project_description_raw,
                                       FALSE,
                                       TRUE,
                                       TRUE),
         estimated_cost_change = if_else(estimated_cost_clean == estimated_cost_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         update_activity_change = if_else(update_activity_clean == update_activity_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         environmental_assessment_stage_change = if_else(environmental_assessment_stage_clean == environmental_assessment_stage_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         construction_type_change = if_else(construction_type_clean == construction_type_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         construction_subtype_change = if_else(construction_subtype_clean == construction_subtype_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         project_type_change = if_else(project_type_clean ==project_type_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         region_change = if_else(region_clean == region_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         municipality_change = if_else(municipality_clean == municipality_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         developer_change = if_else(developer_clean == developer_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         architect_change = if_else(architect_clean == architect_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         project_status_change = if_else(project_status_clean == project_status_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         project_stage_change = if_else(project_stage_clean == project_stage_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         project_category_name_change = if_else(project_category_name_clean == project_category_name_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         public_funding_ind_change = if_else(public_funding_ind_clean == public_funding_ind_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         provinvial_funding_change = if_else(provinvial_funding_clean == provinvial_funding_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         federal_funding_change = if_else(federal_funding_clean == federal_funding_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         municipal_funding_change = if_else(municipal_funding_clean == municipal_funding_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         other_public_funding_change = if_else(other_public_funding_clean == other_public_funding_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         green_building_ind_change = if_else(green_building_ind_clean == green_building_ind_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         green_building_desc_change = if_else(green_building_desc_clean == green_building_desc_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         clean_energy_ind_change = if_else(clean_energy_ind_clean == clean_energy_ind_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         indigenous_ind_change = if_else(indigenous_ind_clean == indigenous_ind_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         indigenous_names_change = if_else(indigenous_names_clean == indigenous_names_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         indigenous_agreement_change = if_else(indigenous_agreement_clean == indigenous_agreement_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         construction_jobs_change = if_else(construction_jobs_clean == construction_jobs_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         operating_jobs_change = if_else(operating_jobs_clean == operating_jobs_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         standardized_start_date_change = if_else(standardized_start_date_clean == standardized_start_date_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         standardized_completion_date_change = if_else(standardized_completion_date_clean == standardized_completion_date_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         latitude_change = if_else(latitude_clean == latitude_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         longitude_change = if_else(longitude_clean == longitude_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         latitude_dms_change = if_else(latitude_dms_clean == latitude_dms_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         longitude_dms_change = if_else(longitude_dms_clean == longitude_dms_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         telephone_change = if_else(telephone_clean == telephone_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         project_website_change = if_else(project_website_clean == project_website_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         first_entry_date_change = if_else(first_entry_date_clean == first_entry_date_raw,
                           FALSE,
                           TRUE,
                           TRUE),
         last_update_change = if_else(last_update_clean ==last_update_raw,
                           FALSE,
                           TRUE,
                           TRUE)
  )

colnames(mpi_clean) <- str_to_upper(colnames(mpi_clean))
write_csv(mpi_clean, here::here("processed_data","mpi_clean.csv"))

colnames(mpi_clean_current) <- str_to_upper(colnames(mpi_clean_current))
write_csv(mpi_clean_current, here::here("processed_data","mpi_clean_current.csv"))

colnames(mpi_raw_current) <- str_to_upper(colnames(mpi_raw_current))
write_csv(mpi_raw_current, here::here("processed_data","mpi_raw_current.csv"))

colnames(raw_and_clean_current) <- str_to_upper(colnames(raw_and_clean_current))
write_csv(raw_and_clean_current, here::here("processed_data","raw_and_clean_current.csv"))











