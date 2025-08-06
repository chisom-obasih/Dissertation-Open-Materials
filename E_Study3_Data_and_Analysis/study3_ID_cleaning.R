# Participant ID Cleaning for Study 3 of Dissertation
# Chisom Obasih
# June 2025

# create new ID mappings from privateIDs based on consent forms, and also collect Japanese course info



library(rio)
library(tidyverse)
# Load in consent forms
# These are the VAS data that includes all 103 participants
consent_files <-  list.files(path = "raw_data/questionnaires", pattern = "consent", all.files = FALSE, full.names = TRUE, recursive = FALSE)

# From the single list of data file names, iteratively read each csv files into a single data frame
consent_df <- consent_files %>%
  map_df(~import(.x, fill =TRUE)) %>%
  filter(`Event Index` != "END OF FILE")
  
unique(consent_df$`Experiment ID`)
# [1] 220234 220732 220221
# 220234 = control group, 220732 = highLD group, 220221 = lowLD group

# set seed (have to highlight both of these to get the same order)
set.seed(1738)
study3_IDs_courses <- consent_df %>%
  select(privateID = `Participant Private ID`,
         question = Question,
         course = Response,
         object_name = `Object Name`,
         exp_group = `Experiment ID`) %>%
  filter(object_name == "Course") %>%
  mutate(exp_group = case_when(
    exp_group == "220234" ~ "Control",
    exp_group == "220732" ~ "High LD",
    exp_group == "220221" ~ "Low LD",
    .default = NA
  )) %>%
  select(-c(question, object_name)) %>%
  group_by(privateID = factor(privateID, levels = sample(unique(privateID)))) %>%
  mutate(ID = cur_group_id(), privateID = as.factor(privateID)) %>%
  arrange(ID) %>%
  mutate(ID = as.factor(ID), exp_group = factor(exp_group, levels = c("Control", "Low LD", "High LD"))) %>%
  relocate(exp_group, .after = everything()) %>%
  ungroup()

# save dataframe as an rds object to be loaded into the other data cleaning files (study3_data_cleaning_visualization.Rmd and study3_questionnaire_cleaning.Rmd)
saveRDS(study3_IDs_courses, file = "Rdata/study3_clean_IDs.rds")
