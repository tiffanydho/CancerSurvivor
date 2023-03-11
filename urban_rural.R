pacman::p_load(readxl, tidyverse, ggpubr, rstatix)
library("readxl")

setwd("C:/Users/tiffa/Documents/Research/CancerSurvivor")
filename <- "Cancer_cohort_deidentified.xlsx"
filename_test <- "test.xlsx"
t <- read_excel(filename_test, sheet = 2,col_names = TRUE, col_types = NULL, na = "NA", skip = 0)

raw_data <- read_excel(filename, col_names = TRUE, col_types = NULL, na = "NA", skip = 0)
# remove rows with at least 1 NA (58 obs): 431 obs --> 373 obs
# handle gender, race as if only one choice is selected => need to investigate how many had multiple selection & what to do with them
# To Do: handle the 2 variables about "sleep time"

# df_selected
df.s <- raw_data %>% select(
  "rural", "age", "genderc___1",	"genderc___2", "race___1", "race___2", "race___3", "race___4", "race___5", 
  "race___6", "race___7", "race___8", "race___9", "race___10", "race___11", "race___12", "race___13", "race___14", 
  "race___15", "education", "BMI", "smoke100", "sleepquality7d", "wdaysleep_hrmin", "wendsleep_hrmin") %>% 
  na.omit() 

# df_cleaned
df.c <- df.s %>% mutate(gender = ifelse(genderc___1 == 1, "male", "female"), .after = age) %>% 
  mutate(race=case_when(race___1 == 1 ~ "White", race___2 == 1 ~ "Black", 
                        race___3 == 1 ~ "Native American", 
                        race___4 == 1 | race___5 == 1 | race___6 == 1|race___7 == 1|race___8 == 1|race___9 == 1|race___10 == 1 ~ "Asian",
                        race___11 == 1 | race___12 == 1 | race___13 == 1|race___14 == 1 ~ "Islander", 
                        race___15 == 1 ~ "Other" ), .after = gender) %>% 
  select(-contains(c("genderc___", "race___")))


f_test <- read_excel(filename_test, col_names = TRUE, col_types = NULL, na = "", skip = 0)
# multi-race not handled
t <- t %>% mutate(race=case_when(race___1 == 1 ~ "White", race___2 == 1 ~ "Black", 
                                 race___3 == 1 ~ "Native American", 
                                 race___4 == 1 | race___5 == 1 | race___6 == 1|race___7 == 1|race___8 == 1|race___9 == 1|race___10 == 1 ~ "Asian",
                                 race___11 == 1 | race___12 == 1 | race___13 == 1|race___14 == 1 ~ "Islander", 
                                 race___15 == 1 ~ "Other" )) 




###### Data visualization #######
### Demographics ###
# Age: 
# check to make sure only one is selected: sum = 1. If multiple is selected, discard the entry. 


temp <- f_test %>% mutate(gender = if_else(genderc___1==1, "male", "female"))


