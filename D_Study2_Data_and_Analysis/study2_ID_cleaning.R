# Participant ID Cleaning for Study 2 of Dissertation
# Chisom Obasih
# April 2025

# n = 7 participants (out of the total n = 103) had to run Day 2 on a separate Gorilla experiment from Day 1, due to a mistake in posting Day 2 too early on Prolific (and the mistake of making the completion the same between Day 1 and Day 2)
# Because of running the study on two different Gorilla experiments, the public ID for these 7 participants were matched with two different Private IDs in the blinded data
# In order to match the public and private IDs for these 7, I systematically assigned all 103 participants a new ID based on public ID (101 - 203)
# This is so that I can resave the data BLINDED (so that there is no public ID) and use a saved dataframe to match the private IDs (even the separate private IDs) with the singular new ID

# Because this code loads in the unblinded data for one Task, and then the unblinded data was deleted from my computer and replaced with the blinded data from Gorilla, this means that this code is NOT able to be reproduced exactly. It is included in the project for transparency.
# This was also for the sake of removing participants' identifiable information (public Prolific ID) and following approved IRB protocol for data management.


library(rio)
library(tidyverse)
# Load in an unblinded data (now deleted from my computer)
# These are the VAS data that includes all 103 participants
VAS_files <- list.files(path = "raw_data", pattern = "*VAS*", all.files = FALSE, full.names = TRUE, recursive = FALSE)

# From the single list of data file names, iteratively read each csv files into a single data frame
VAS_df <- VAS_files %>%
  map_df(~import(.x))

clean_IDs <- VAS_df %>%
  select(
  `Local Date and Time`,
  `Participant Public ID`,
  `Participant Private ID`) %>%
  clean_names() %>%
  rename(publicID = participant_public_id, privateID = participant_private_id) %>%
  arrange(publicID, local_date_and_time) %>% # it is also necessary to identify day 1 and day 2
  # some participants had to do day 2 of the study on a separate experiment due to a mistake linking Gorilla and Prolific, so I need to match the two different private IDs to matching publicIDs of day 1 and day 2, I'll do this by making a new ID based on the publicIDs
  group_by(publicID) %>%
  mutate(ID = (cur_group_id() + 100), .after = privateID) %>%
  select(-local_date_and_time) %>%
  distinct() %>%
  # at this point, the dataframe has 110 rows, which aligns with the fact that 96 participants have just one row while 7 participants have two rows (two private ID values, but matching publicID and new ID values across the two rows), so this worked
  # remove publicID from the dataframe, then save it as an R object that will be loaded into the datacleaning files and used to match privateIDs
  # and save the columns as factors
  ungroup() %>%
  select(-publicID) %>%
  mutate(privateID = as.factor(privateID), ID = as.factor(ID))

# and now we can see that the participants with two privateIDs are 136, 160, 168, 175, 179, 194, and 199

# now save this dataframe as an Rdata object to be loaded into the other data cleaning files (study2_data_cleaning_visualization.Rmd and study2_questionnaire_cleaning.Rmd)
save(clean_IDs, file = "Rdata/clean_IDs.Rdata")

