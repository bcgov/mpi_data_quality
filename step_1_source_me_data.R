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

# This script downloads the MPI data from https://www2.gov.bc.ca/gov/content/employment-business/economic-development/industry/bc-major-projects-inventory/recent-reports
# and does some pre-processing.
# libraries--------
if (!"tidyverse" %in% names(sessionInfo()$otherPkgs)) library(tidyverse)
if (!"lubridate" %in% names(sessionInfo()$otherPkgs)) library(lubridate) # years and months not exported objects
source(here::here("R", "functions.R"))

if (!file.exists(here::here("raw_data"))) dir.create(here::here("raw_data"))
mpi_url_to_scrape <- "https://www2.gov.bc.ca/gov/content/employment-business/economic-development/industry/bc-major-projects-inventory/recent-reports"
mpi_scraped <- rvest::read_html(mpi_url_to_scrape)
mpi_links <- rvest::html_attr(rvest::html_nodes(mpi_scraped, "a"), "href") # all the links
mpi_links <- mpi_links[mpi_links %>% startsWith("/assets/") & mpi_links %>% endsWith(".xlsx")] %>% # stubs of the links we want.
  na.omit()
mpi_links <- paste0("https://www2.gov.bc.ca", mpi_links) # paste the head onto the stubs
mpi_files <- paste0("mpi_dl", 1:length(mpi_links), ".xlsx") # sane file naming.
mapply(download.file, mpi_links, here::here("raw_data", mpi_files)) # downloads all the mpi files into folder raw_data
mpi_all_sheets <- sapply(here::here("raw_data", mpi_files), readxl::excel_sheets) # gets all the sheets
sheet_starts_with_mpi <- lapply(mpi_all_sheets, function(x) x[startsWith(x, "mpi")]) %>%
  unlist(use.names = FALSE) # could break... assumes excel sheet naming remains consistent.
sheet_starts_with_Full <- lapply(mpi_all_sheets, function(x) x[startsWith(x, "Full")]) %>%
  unlist(use.names = FALSE)
# file structure changed significantly in 2016... only use recent files--------
mpi_sheets <- c(sheet_starts_with_mpi, sheet_starts_with_Full)
mpi_files <- mpi_files[1:length(mpi_sheets)] # could break... assumes excel sheet naming remains consistent.
mpi_nested <- tibble(file = here::here("raw_data", mpi_files), sheet = mpi_sheets) %>%
  mutate(
    data = map2(file, sheet, readxl::read_excel),
    data = map(data, janitor::clean_names),
    data = map(data, keep_columns)
  )
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
saveRDS(mpi_raw, here::here("processed_data", "mpi_raw.rds"))

# do the same for Man's long file-----------

mpi_raw_long <- readxl::read_excel(here::here("raw_data", "MPIlongraw.xlsx"),
  col_types = c(
    "numeric",
    "text",
    "numeric",
    "text",
    "text",
    "text",
    "text",
    "text",
    "date",
    "text",
    "text",
    "date"
  )
) %>%
  janitor::clean_names() %>%
  mutate(published_dates = lubridate::floor_date(published_dates, unit = "month")) # some of the dates messed up.

saveRDS(mpi_raw_long, here::here("processed_data", "mpi_raw_long.rds"))
