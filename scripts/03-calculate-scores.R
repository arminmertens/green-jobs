## ---------------------------
##
## Purpose of script: Calculate green jobs scores for each job and sector
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

keywords <- read_csv('https://raw.githubusercontent.com/johannabi/WAoeN/main/data/WAoeN.csv') %>% 
  janitor::clean_names()

green_skills <- read_rds('data/green_skills_extr.rds')

green_skills <- green_skills %>% 
  mutate(green_keywords = trimws(green_keywords),
         # Horrible solution, fix later when time!!
         green_keywords = case_when(green_keywords %in% c("/pp/", "pp/", "-pp/",
                                                          "/pp", "pp-", "-pp-",
                                                          "/pp-", "-pp") ~ "pp",
                                    green_keywords %in% c("-led", "led-", 
                                                          "-led-", "-led/", 
                                                          "led/", "/led")
                                    ~"led",
                                    green_keywords == "enev-" ~ "enev",
                                    green_keywords == "lein-" ~ "lein",
                                    green_keywords %in% c("velo-", "velo/") 
                                    ~ "velo",
                                    green_keywords %in% c("ökologi", "ökologe")
                                    ~ "ökolog.",
                                    TRUE ~ green_keywords)) %>% 
  left_join(keywords %>% mutate(begriff = tolower(begriff)),
                                by = c("green_keywords" = "begriff")) 

green_job_scores <- 
  green_skills %>% 
  group_by(posting_id) %>% 
  summarise(sum = sum(assoziations_score))

job_ads <- 
  job_ads %>% 
  left_join(green_job_scores, by = "posting_id") %>% 
  mutate(sum = replace_na(sum, 0))

job_ads %>% 
  drop_na(organization_activity.label) %>% 
  group_by(organization_activity.label) %>% 
  summarise(score = sum(sum) / n()) %>% 
  writexl::write_xlsx("data/green_jobs_sector.xlsx")