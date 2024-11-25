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

# This script makes plots.

# libraries--------
if (!"tidyverse" %in% names(sessionInfo()$otherPkgs)) library(tidyverse)
if (!"lubridate" %in% names(sessionInfo()$otherPkgs)) library(lubridate) # years and months not exported objects
source(here::here("R", "functions.R"))
mpi_raw <- readRDS(here::here("processed_data", "mpi_raw.rds")) %>%
  mutate(data_type = "raw")
mpi_clean <- readRDS(here::here("processed_data", "mpi_clean.rds")) %>%
  mutate(data_type = "clean") %>%
  select(-weight)
mpi_fabricated <- readRDS(here::here("processed_data", "mpi_fabricated.rds")) %>%
  mutate(data_type = "fabricated") %>%
  select(-weight)

all_data <- bind_rows(mpi_raw, mpi_clean, mpi_fabricated)

plots <- tibble(facet_by = c("construction_type", "construction_subtype", "project_type", "region", "project_status", "project_stage", "project_category_name")) %>%
  mutate(plot = map(facet_by, plot_diff))

saveRDS(plots, here::here("processed_data", "plots.rds"))
