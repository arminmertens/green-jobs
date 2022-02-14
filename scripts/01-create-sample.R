## ---------------------------
##
## Purpose of script: Create Random sample of Textkernel data for Green jobs
##
## Author: Armin Mertens
##
## Date Created: 2022-02-10
##
## Copyright (c) Armin Mertens, 2022
## Email: armin.mertens@iwkoeln.de
##
## ---------------------------

# Load Dependencies
library(tidyverse)

# Create random sample
create_sample <- function(data, ratio) {
  set.seed(73)
  jobs_sample <- 
    data %>% 
    # sample data 
    sample_n(round(nrow(data) * ratio, 0)) %>% 
    # select only relevant features
    select(posting_id, date, via_intermediary, job_title, organization_name,
           full_text, profession.label, profession_group.label,
           profession_class.label,organization_activity.label, location_name)
  return(jobs_sample)
}

# Export data to .rds
export_sample <- function(data, filename) {
  data %>% 
    write_rds(paste0("data/", filename, ".rds"))
}


# Read Textkernel data for 2021
# Data is already deduplicated (see project: KI-Monitor)
# Large file ~3.6GB
jobs_sample <- 
  # create_random_sample
  create_sample(data = read_rds("C://Daten/Textkernel/data_distinct_2021.rds"), 
                ratio = 0.1) %>% 
  # export data
  export_sample("green-jobs")
