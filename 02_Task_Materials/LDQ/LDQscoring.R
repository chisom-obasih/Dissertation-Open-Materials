# Linguistic Diversity Questionnaire Scoring Functions
# Written by Chisom Obasih, April 2025
# Functions used to calculate Multilingual Diversity (MLD) scores from Parts 1 and 2 of Linguistic Diversity Questionnaire (LDQ), used in dissertation


# Originally written in the study1_questionnaire_scoring.Rmd file in the Study1/data_cleaning_visualization_analysis folder
# This script can't be used by itself to score anything, running this script by itself as it is will do nothing but save the functions
# But the functions can be sourced from here to be called in other Rmd files

########################## Load libraries ##########################
suppressPackageStartupMessages(library(tidyverse))
library(janitor)
library(rio)
library(ggthemes)
library(ggpubr)
library(knitr)

library(conflicted)
# package to solve conflicts between functions of different packages that use the same name
# use dplyr for all functions in the case of conflict between packages, output suppressed
conflict_prefer_all("dplyr", quiet = TRUE)


# avoid scientific notation
options(scipen = 999)


########################## LDQ Part 1 ##########################

# Create function to calculate aggregate scores using LHQ3 aggregate scores equations from here:
# https://lhq-blclab.org/static/docs/aggregate-scores.html


# calculates language proficiency, language immersion, language dominance, ratio of dominance, proportion of dominance, and multilingual language diversity (MLD)
# returns data frame with the values for each of these scores per language per participant
calculate_active_scores <- function(df, weights, K) {
  
  
  ### ensure correct arguments ###
  
  # require dataframe
  if (missing(df)){
    stop("This function requires a dataframe.")
  }
  
  # make sure it is a dataframe
  
  if(!(is.data.frame(df))){
    stop("Argument `df` must be a dataframe.")
  }
  
  
  # if weights argument is missing, set to default of 0.25 for all keys and print message
  if (missing(weights)){
    weights = list("R" = 0.25, "L" = 0.25, "W" = 0.25, "S" = 0.25)
    print(quote = FALSE, "Argument `weights` has been set to default: weights = list(\"R\" = 0.25, \"L\" = 0.25, \"W\" = 0.25, \"S\" = 0.25).")
  }
  
  # weights must be a list with keys L, W, S, R
  # if the supplied argument for weights is not a list, throw error
  # if the list does not have the exact names R, L, W, S (in any order), throw error
  # this is evaluated with the %in% evaluation (which returns a vector of bools), converted to numeric, and then the sum of the numeric output should be to 4 if there are four keys in the list and they match the names R, L, W, S
  # so this will throw an error if there are four keys but they don't match the list, or if there are more or less than keys
  if(typeof(weights) != "list" | sum(as.numeric(names(weights) %in% c("L", "W", "R", "S"))) != 4){
    stop("Argument `weights` must be of type list with the names R, L, W, S.")
  }
  
  # weights must equal 1
  if(sum(c(weights$R, weights$L, weights$W, weights$S)) != 1){
    stop("The values of the argument `weights` must equal to 1.")
  }
  
  # if K argument is missing, set to default of 16 and print message
  if(missing(K)){
    K = 16
    print(quote = FALSE, "Argument `K` has been set to default: K = 16.")
  }
  
  ### set up output ###
  
  # create data frame to hold the scores
  # output data frame should have the same number of rows as input data frame
  n = nrow(df)
  output <- data.frame(ID = character(n),
                       age = double(n),
                       gender = character(n),
                       language1 = character(n),
                       language2 = character(n),
                       language3 = character(n),
                       language4 = character(n),
                       proficiency1 = double(n),
                       proficiency2 = double(n),
                       proficiency3 = double(n),
                       proficiency4 = double(n),
                       immersion1 = double(n),
                       immersion2 = double(n),
                       immersion3 = double(n),
                       immersion4 = double(n),
                       dominance1 = double(n),
                       dominance2 = double(n),
                       dominance3 = double(n),
                       dominance4 = double(n),
                       ratio1 = double(n),
                       ratio2 = double(n),
                       ratio3 = double(n),
                       ratio4 = double(n),
                       PD1 = double(n),
                       PD2 = double(n),
                       PD3 = double(n),
                       PD4 = double(n),
                       MLD_A = double(n))
  
  ### set up for loop to go through each row of input dataframe ###
  
  # for this function to work, the input df must have the exact same columns as indicated below
  for (i in 1:nrow(df)){
    
    
    # copy necessary info from df to output
    output$ID[i] = df$ID[i]
    output$age[i] = df$age[i]
    output$gender[i] = df$gender[i]
    output$language1[i] = df$language1[i]
    output$language2[i] = df$language2[i]
    output$language3[i] = df$language3[i]
    output$language4[i] = df$language4[i]
    
    
    ### calculate proficiency scores ###
    
    # empty vector for output of proficiency equation, each element of the vector will be the proficiency score for the i-th language
    prof <- c()
    
    # save the proficiency scores into different vectors depending on linguistic aspect
    p_r <- c(df$proficiency_R_lang1[i], 
             df$proficiency_R_lang2[i], 
             df$proficiency_R_lang3[i], 
             df$proficiency_R_lang4[i])
    p_l <- c(df$proficiency_L_lang1[i], 
             df$proficiency_L_lang2[i],
             df$proficiency_L_lang3[i],
             df$proficiency_L_lang4[i])
    p_w <- c(df$proficiency_W_lang1[i], 
             df$proficiency_W_lang2[i], 
             df$proficiency_W_lang3[i],
             df$proficiency_W_lang4[i])
    p_s <- c(df$proficiency_S_lang1[i], 
             df$proficiency_S_lang2[i], 
             df$proficiency_S_lang3[i], 
             df$proficiency_S_lang4[i])
    
    # for loop equation to calculate proficiency score for each of the four languages, each saved as an element in the vector named prof
    for (j in 1:4){
      prof[j] <- (1/7) * sum(
        (weights$R * p_r[j]), 
        (weights$L * p_l[j]), 
        (weights$W * p_w[j]), 
        (weights$S * p_s[j]), 
        na.rm = TRUE)
    }
    
    # save the proficiency equation outputs into their respective columns
    output$proficiency1[i] = prof[1]
    output$proficiency2[i] = prof[2]
    output$proficiency3[i] = prof[3]
    output$proficiency4[i] = prof[4]
    
    
    ### calculate immersion scores ###
    
    # empty vector for output of immersion equation, each element of the vector will be the immersion score for the i-th language
    imm <- c()
    
    # save the AoA (age of acquisition) and YoU (years of use) scores into different vectors depending on linguistic aspect (YoU is its own vector)
    aoa_r <- c(df$aoa_R_lang1[i], 
               df$aoa_R_lang2[i], 
               df$aoa_R_lang3[i], 
               df$aoa_R_lang4[i])
    aoa_l <- c(df$aoa_L_lang1[i], 
               df$aoa_L_lang2[i], 
               df$aoa_L_lang3[i], 
               df$aoa_L_lang4[i])
    aoa_w <- c(df$aoa_W_lang1[i], 
               df$aoa_W_lang2[i], 
               df$aoa_W_lang3[i], 
               df$aoa_W_lang4[i])
    aoa_s <- c(df$aoa_S_lang1[i], 
               df$aoa_S_lang2[i], 
               df$aoa_S_lang3[i], 
               df$aoa_S_lang4[i])
    you <- c(df$aoa_YoU_lang1[i], 
             df$aoa_YoU_lang2[i], 
             df$aoa_YoU_lang3[i], 
             df$aoa_YoU_lang4[i])
    
    # save age of current row's participant into a simpler variable
    age_i = df$age[i]
    
    # for loop equation to calculate immersion score for each of the four languages, each saved as an element in the vector named imm
    for (j in 1:4){
      imm[j] <- (1/2) * sum(
        (weights$R * ((age_i - aoa_r[j])/age_i)), 
        (weights$L * ((age_i - aoa_l[j])/age_i)), 
        (weights$W * ((age_i - aoa_w[j])/age_i)), 
        (weights$S * ((age_i - aoa_s[j])/age_i)),
        (you[j]/age_i), na.rm = TRUE)
    }
    
    # save the immersion equation outputs into their respective columns
    output$immersion1[i] = imm[1]
    output$immersion2[i] = imm[2]
    output$immersion3[i] = imm[3]
    output$immersion4[i] = imm[4]
    
    
    ### calculate dominance scores ###
    
    # empty vector for output of dominance equation, each element of the vector will be the dominance score for the i-th language
    dom <- c()
    
    # save the time spent on activities into different vectors depending on linguistic aspect
    
    # reading aspect per language is sum of "Reading for fun" and "Reading for work/school" (question 18 on LHQ3)
    hours_r <- c(sum(df$hours_activities_fun_R_lang1[i],
                     df$hours_activities_school_work_R_lang1[i], 
                     na.rm = TRUE), 
                 sum(df$hours_activities_fun_R_lang2[i],
                     df$hours_activities_school_work_R_lang2[i], 
                     na.rm = TRUE), 
                 sum(df$hours_activities_fun_R_lang3[i],
                     df$hours_activities_school_work_R_lang3[i], 
                     na.rm = TRUE), 
                 sum(df$hours_activities_fun_R_lang4[i],
                     df$hours_activities_school_work_R_lang4[i], 
                     na.rm = TRUE))
    # listening aspect per language is sum of "Watching television" and "Listening to radio" (question 18 on LHQ3)
    hours_l <- c(sum(df$hours_activities_TV_L_lang1[i],
                     df$hours_activities_radio_L_lang1[i], 
                     na.rm = TRUE), 
                 sum(df$hours_activities_TV_L_lang2[i],
                     df$hours_activities_radio_L_lang2[i], 
                     na.rm = TRUE), 
                 sum(df$hours_activities_TV_L_lang3[i],
                     df$hours_activities_radio_L_lang3[i], 
                     na.rm = TRUE), 
                 sum(df$hours_activities_TV_L_lang4[i],
                     df$hours_activities_radio_L_lang4[i], 
                     na.rm = TRUE))
    # writing aspect per language is sum of "Using social media and the internet" and "Writing for school/work" (question 18 on LHQ3)
    hours_w <- c(sum(df$hours_activities_social_internet_W_lang1[i], 
                     df$hours_activities_school_work_W_lang1[i], 
                     na.rm = TRUE), 
                 sum(df$hours_activities_social_internet_W_lang2[i], 
                     df$hours_activities_school_work_W_lang2[i], 
                     na.rm = TRUE), 
                 sum(df$hours_activities_social_internet_W_lang3[i], 
                     df$hours_activities_school_work_W_lang3[i], 
                     na.rm = TRUE), 
                 sum(df$hours_activities_social_internet_W_lang4[i], 
                     df$hours_activities_school_work_W_lang4[i], 
                     na.rm = TRUE))
    # speaking aspect per language is sum of hours spent talking to "work_school members", "Friends", "Classmates", and "Others" (question 19 on LHQ3)
    hours_s <- c(sum(df$hours_people_work_school_S_lang1[i], 
                     df$hours_people_friends_S_lang1[i], 
                     df$hours_people_classmates_S_lang1[i], 
                     df$hours_people_others_S_lang1[i], 
                     na.rm = TRUE),
                 sum(df$hours_people_work_school_S_lang2[i], 
                     df$hours_people_friends_S_lang2[i], 
                     df$hours_people_classmates_S_lang2[i], 
                     df$hours_people_others_S_lang2[i], 
                     na.rm = TRUE),
                 sum(df$hours_people_work_school_S_lang3[i], 
                     df$hours_people_friends_S_lang3[i], 
                     df$hours_people_classmates_S_lang3[i], 
                     df$hours_people_others_S_lang3[i], 
                     na.rm = TRUE),
                 sum(df$hours_people_work_school_S_lang4[i],
                     df$hours_people_friends_S_lang4[i],
                     df$hours_people_classmates_S_lang4[i],
                     df$hours_people_others_S_lang4[i], 
                     na.rm = TRUE))
    # this equation also uses the proficiency scores from above
    
    # for loop equation to calculate dominance score for each of the four languages, each saved as an element in the vector named dom
    for (j in 1:4){
      dom[j] <- sum(
        (weights$R * (((1/2)*(p_r[j]/7))+((1/2)*(hours_r[j]/K)))), 
        (weights$L * (((1/2)*(p_l[j]/7))+((1/2)*(hours_l[j]/K)))), 
        (weights$W * (((1/2)*(p_w[j]/7))+((1/2)*(hours_w[j]/K)))), 
        (weights$S * (((1/2)*(p_s[j]/7))+((1/2)*(hours_s[j]/K)))), na.rm = TRUE)
    }
    
    # save the immersion equation outputs into their respective columns
    output$dominance1[i] = dom[1]
    output$dominance2[i] = dom[2]
    output$dominance3[i] = dom[3]
    output$dominance4[i] = dom[4]
    
    ### calculate ratio of dominance for each language ###
    
    # ratio of dominance is compared to first language, so ratio 1 should always equal 1
    output$ratio1[i] = dom[1]/dom[1]
    output$ratio2[i] = dom[2]/dom[1]
    output$ratio3[i] = dom[3]/dom[1]
    output$ratio4[i] = dom[4]/dom[1]
    
    ### calculate proportion of dominance (PD) ###
    
    # create empty vector to hold PD values for each language
    pd = c()
    
    # equation vectorized over each element of dom vector, creating an equal number of elements in the pd vector
    pd = dom/sum(dom)
    
    # save output of PD equation to data frame
    output$PD1[i] = pd[1]
    output$PD2[i] = pd[2]
    output$PD3[i] = pd[3]
    output$PD4[i] = pd[4]
    
    ### calculate multilingual language diversity (MLD) ###
    
    # the pd*log2(pd) equation is vectorized over each element of pd vector, and each of those elements is summed
    output$MLD_A[i] = -1 * sum(pd*log2(pd), na.rm = TRUE)
    
  } # end for loop through df rows
  
  # coerce output columns ID and gender to factors
  # and round numeric values to 2 decimal places
  output <- output %>%
    mutate(ID = as.factor(ID), gender = as.factor(gender)) %>%
    mutate(across(where(is.numeric), ~ round(., 2)))
  
  # function returns the output dataframe
  return(output)
}


########################## LDQ Part 2 ##########################

# calculates language entropy for each of the 10 contexts and calculate aggregate multilingual language diversity (MLD) score for passive language use (MLD-P) based on proportion of 24 hour day spent in context
# returns data frame with the language entropy values per context and one MLD score per participant

calculate_passive_scores <- function(df, K) {
  
  # require dataframe
  if (missing(df)){
    stop("This function requires a dataframe.")
  }
  
  # make sure it is a dataframe
  
  if(!(is.data.frame(df))){
    stop("Argument `df` must be a dataframe.")
  }
  
  
  # if K argument is missing, set to default of 24 and print message
  if(missing(K)){
    K = 24
    print(quote = FALSE, "Argument `K` has been set to default: K = 24.")
  }
  
  # create empty output dataframe - this will be populated in the for loop with rbind
  output <- data.frame()
  
  # create vector of context category names 
  context <- c("watching", "listening", "games", "internet", "home", "family", "social", "work_school", "out_of_home", "other")
  
  
  ### set up for loop to go through each row of input dataframe ###
  
  # for this function to work, the input df must have the exact same columns as indicated below
  for (i in 1:nrow(df)){
    
    # (re)set empty dataframe to hold calculations for the current row of input df
    calculations_i <- data.frame(row.names = context)
    
    # create a vector to save average time spent in each context
    context_hours <- c(df$context_hours_watching[i], 
                       df$context_hours_listening[i], 
                       df$context_hours_games[i], 
                       df$context_hours_internet[i], 
                       df$context_hours_home[i],
                       df$context_hours_family[i],
                       df$context_hours_social[i],
                       df$context_hours_work_school[i],
                       df$context_hours_out_of_home[i],
                       df$context_hours_other[i])
    
    
    # save time spent on each language within each context into different vectors depending on context
    watching_per_lang <- c(df$passive_watching_lang1_hours[i], 
                           df$passive_watching_lang2_hours[i], 
                           df$passive_watching_lang3_hours[i], 
                           df$passive_watching_lang4_hours[i])
    listening_per_lang <- c(df$passive_listening_lang1_hours[i], 
                            df$passive_listening_lang2_hours[i], 
                            df$passive_listening_lang3_hours[i], 
                            df$passive_listening_lang4_hours[i])
    games_per_lang <- c(df$passive_games_lang1_hours[i], 
                        df$passive_games_lang2_hours[i], 
                        df$passive_games_lang3_hours[i], 
                        df$passive_games_lang4_hours[i])
    internet_per_lang <- c(df$passive_internet_lang1_hours[i], 
                           df$passive_internet_lang2_hours[i], 
                           df$passive_internet_lang3_hours[i], 
                           df$passive_internet_lang4_hours[i])
    home_per_lang <- c(df$passive_home_lang1_hours[i], 
                       df$passive_home_lang2_hours[i], 
                       df$passive_home_lang3_hours[i], 
                       df$passive_home_lang4_hours[i])
    family_per_lang <- c(df$passive_family_lang1_hours[i], 
                         df$passive_family_lang2_hours[i], 
                         df$passive_family_lang3_hours[i], 
                         df$passive_family_lang4_hours[i])
    social_per_lang <- c(df$passive_social_lang1_hours[i], 
                         df$passive_social_lang2_hours[i], 
                         df$passive_social_lang3_hours[i], 
                         df$passive_social_lang4_hours[i])
    work_school_per_lang <- c(df$passive_work_school_lang1_hours[i], 
                              df$passive_work_school_lang2_hours[i], 
                              df$passive_work_school_lang3_hours[i], 
                              df$passive_work_school_lang4_hours[i])
    out_of_home_per_lang <- c(df$passive_out_of_home_lang1_hours[i], 
                              df$passive_out_of_home_lang2_hours[i], 
                              df$passive_out_of_home_lang3_hours[i], 
                              df$passive_out_of_home_lang4_hours[i])
    other_per_lang <- c(df$passive_other_lang1_hours[i], 
                        df$passive_other_lang2_hours[i], 
                        df$passive_other_lang3_hours[i], 
                        df$passive_other_lang4_hours[i])
    
    
    # create vectors of the proportions of each language reported for each context, divided by the total hours reported for that context (division is vectorized over the per_lang vectors)
    watching_prop_per_lang = watching_per_lang/sum(watching_per_lang, na.rm = T)
    listening_prop_per_lang = listening_per_lang/sum(listening_per_lang, na.rm = T)
    games_prop_per_lang = games_per_lang/sum(games_per_lang, na.rm = T)
    internet_prop_per_lang = internet_per_lang/sum(internet_per_lang, na.rm = T)
    home_prop_per_lang = home_per_lang/sum(home_per_lang, na.rm = T)
    family_prop_per_lang = family_per_lang/sum(family_per_lang, na.rm = T)
    social_prop_per_lang = social_per_lang/sum(social_per_lang, na.rm = T)
    work_school_prop_per_lang = work_school_per_lang/sum(work_school_per_lang, na.rm = T)
    out_of_home_prop_per_lang = out_of_home_per_lang/sum(out_of_home_per_lang, na.rm = T)
    other_prop_per_lang = other_per_lang/sum(other_per_lang, na.rm = T)
    
    
    # create a vector containing the number of (non-0) languages reported in each context
    context_lang_sum <- c(sum(!is.na(watching_per_lang) & watching_per_lang != 0), 
                          sum(!is.na(listening_per_lang) & listening_per_lang != 0),
                          sum(!is.na(games_per_lang) & games_per_lang != 0),
                          sum(!is.na(internet_per_lang) & internet_per_lang != 0),
                          sum(!is.na(home_per_lang) & home_per_lang != 0),
                          sum(!is.na(family_per_lang) & family_per_lang != 0),
                          sum(!is.na(social_per_lang) & social_per_lang != 0),
                          sum(!is.na(work_school_per_lang) & work_school_per_lang != 0),
                          sum(!is.na(out_of_home_per_lang) & out_of_home_per_lang != 0),
                          sum(!is.na(other_per_lang) & other_per_lang != 0))
    
    # populate context_entropies vector with all the entropy calculations per context
    # the equation for language entropy per context is vectorized over the 4 elements (languages) per context
    context_entropies <- c(
      (-1 * sum(watching_prop_per_lang *log2(watching_prop_per_lang), na.rm = T)),
      (-1 * sum(listening_prop_per_lang *log2(listening_prop_per_lang), na.rm = T)),
      (-1 * sum(games_prop_per_lang *log2(games_prop_per_lang), na.rm = T)),
      (-1 * sum(internet_prop_per_lang *log2(internet_prop_per_lang), na.rm = T)),
      (-1 * sum(home_prop_per_lang *log2(home_prop_per_lang), na.rm = T)),
      (-1 * sum(family_prop_per_lang *log2(family_prop_per_lang), na.rm = T)),
      (-1 * sum(social_prop_per_lang *log2(social_prop_per_lang), na.rm = T)),
      (-1 * sum(work_school_prop_per_lang *log2(work_school_prop_per_lang), na.rm = T)),
      (-1 * sum(out_of_home_prop_per_lang *log2(out_of_home_prop_per_lang), na.rm = T)),
      (-1 * sum(other_prop_per_lang *log2(other_prop_per_lang), na.rm = T)))
    
    # add to calculations
    calculations_long <- calculations_i %>%
      # convert rownames to a column for context names
      rownames_to_column("context") %>%
      # add column for number of reported non-0 languages per context
      mutate(langs_per_context = context_lang_sum) %>%
      # add the vector of the context entropies as a column to the dataframe
      mutate(context_entropies = context_entropies) %>%
      # add column for context hours per day 
      mutate(context_hours = context_hours) %>%
      # add column for scaled weights of each context by dividing the hours per day by the scaling factor K (default K = 24)
      mutate(scaled_proportions = context_hours/K) %>%
      # add ID
      mutate(ID = df$ID[i], .before = "context") %>%
      # calculate passive multilingual language diversity (MLD_P) score as a weighted entropy score - the sums of the entropy per context by the proportion of time awake spent in that context - use context_proportions_alt
      mutate(MLD_P = sum(scaled_proportions * context_entropies, na.rm = T))
    
    # pivot dataframe wider to be one row, that will be added to output dataframe
    calculations_wide <- calculations_long %>%
      pivot_wider(id_cols = c(ID, MLD_P), names_from = context, values_from = c(context_hours, scaled_proportions, langs_per_context, context_entropies), names_vary = "slowest") %>%
      # add age and gender to row
      mutate(age = df$age[i], .after = ID) %>%
      mutate(gender = df$gender[i], .after = age)
    
    # output was initialized outside of for loop, so it should just grow with each iteration
    output = rbind(output, calculations_wide)
    
  } # end for loop
  
  # coerce output columns ID and gender to factors
  # and round numeric values to 2 decimal places
  output <- output %>%
    mutate(ID = as.factor(ID), gender = as.factor(gender)) %>%
    mutate(across(where(is.numeric), ~ round(., 2)))
  
  # function returns the output dataframe
  return(output)
  
}
