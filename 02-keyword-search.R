## ---------------------------
##
## Purpose of script: Search for green job keywords in job ad full texts
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

# Read job ad data
job_ads <- read_rds('data/green-jobs.rds')

regex_term <- function(term) {
  regex_term <-
    paste0("[ /-]", term, "[ /-]")
  return(regex_term)
}

# Read keyword lists
keywords <- read_csv('https://raw.githubusercontent.com/johannabi/WAoeN/main/data/WAoeN.csv') %>% 
  janitor::clean_names() %>% 
  mutate(begriff = case_when(tolower(begriff) == "pp" ~ regex_term("pp"),
                              tolower(begriff) == "lein" ~ regex_term("lein"),
                              tolower(begriff) == "enev" ~ regex_term("enev"),
                              tolower(begriff) == "velo"~ regex_term("velo"),
                             tolower(begriff) == "led"~ regex_term("led"),
                             TRUE ~ begriff))

keyword_search <- function(data) {
  green_jobs <-
    data %>%
    mutate(green_keywords = 
             str_extract_all(tolower(full_text),
                             paste(tolower(keywords$begriff),
                                           collapse = '|'))) %>% 
    select(posting_id, green_keywords)
  return(green_jobs)
}

extract_green_skills <- function(data) {
  skills <- keyword_search(data) %>% 
    unnest(green_keywords)
  return(skills)
}

green_skills <- extract_green_skills(job_ads)