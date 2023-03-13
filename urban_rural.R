pacman::p_load(readxl, tidyverse, ggpubr, rstatix, magrittr, ggplot2)
library("readxl")

setwd("C:/Users/tiffa/Documents/Research/CancerSurvivor/DataAnalysis")
filename <- "Cancer_cohort_deidentified.xlsx"

raw_data <- read_excel(filename, col_names = TRUE, col_types = NULL, na = "NA", skip = 0)
# remove rows with at least 1 NA (58 obs): 431 obs --> 373 obs
# currently handle gender, race as if only one choice is selected => need to investigate how many had multiple selection & what to do with them
# To Do: handle the 2 variables about "sleep time"

# df_s: select columns of interest
df.s <- raw_data %>% select(
  "rural", "age", "genderc___1",	"genderc___2", "race___1", "race___2", "race___3", "race___4", "race___5", 
  "race___6", "race___7", "race___8", "race___9", "race___10", "race___11", "race___12", "race___13", "race___14", 
  "race___15", "education", "BMI", "smoke100", "sleepquality7d", "wdaysleep_hrmin", "wendsleep_hrmin") %>% 
  na.omit() 

# df_c: clean & transform data from binary to categories 
# **multi-race not handled
# sleepquality7d: one entry has "23" as response - removed the entry. Confirm how we'd like to handle
## Will discuss a plan & maybe assign either 2 or 3 to still keep the entry

df.c <- df.s %>% mutate(gender = ifelse(genderc___1 == 1, "male", "female"), .after = age) %>% 
  mutate(race=case_when(race___1 == 1 ~ "White", race___2 == 1 ~ "Black", 
                        race___3 == 1 ~ "Native American", 
                        race___4 == 1 | race___5 == 1 | race___6 == 1|race___7 == 1|race___8 == 1|race___9 == 1|race___10 == 1 ~ "Asian",
                        race___11 == 1 | race___12 == 1 | race___13 == 1|race___14 == 1 ~ "Islander", 
                        race___15 == 1 ~ "Other" ), .after = gender) %>% 
  mutate(rural = recode(rural, "0" = "urban", "1" = "rural")) %>% 
  mutate(education = recode(education, "1" = "< 8 yrs", "2" = "8-11 yrs", 
                            "3" = "12 yrs", "4" = "vocational", "5" = "some college", 
                            "6" = "college grad", "7" = "postgrad")) %>% 
  mutate(smoke100 = recode(smoke100, "0" = "no", "1" = "yes")) %>% 
  mutate(sleepquality7d = recode(sleepquality7d, "1" = "very good", "2" = "good", 
                                 "3" = "fair", "4" = "poor", "5" = "very poor")) %>% 
  select(-contains(c("genderc___", "race___"))) %>% 
  filter(! sleepquality7d == "23") # tentatively remove the entry 

category_cols <- c("rural", "gender", "race", "education", "smoke100", "sleepquality7d")
df.c %<>% mutate_at(category_cols, factor) # apply factor() to the categorical variables

###### Data Summary #############
### overall data ###
summary(df.c)
df.c %>% group_by(rural) %>% get_summary_stats(age, type = "common")
df.c %>% group_by(rural) %>% get_summary_stats(BMI, type = "common")

### Continuous Vars ###
# https://quantifyinghealth.com/summarize-categorical-data-in-r/
library(scales) # to show percentages on the y-axis
table(df.c$gender, df.c$rural) %>% prop.table(margin = 2) # denominator = row sum
table(df.c$rural, df.c$gender)

### Categorical Vars ###
# select column using $ and string
# https://stackoverflow.com/questions/18222286/dynamically-select-data-frame-columns-using-and-a-character-value
for(col.name in category_cols[-1]){
  t <- table(df.c$rural, df.c[[col.name]]) %>% prop.table(margin = 1)
  print(t)
}

###### Data Vis: Continuous Vars ################
ggplot(df.c, aes(x=rural, y=age, fill=rural)) + geom_boxplot() +
  ggtitle("Boxplot of Age by Residency") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Residency") +ylab("Age") + labs(fill = "Residency")

ggplot(df.c, aes(x=rural, y=BMI, fill=rural)) + geom_boxplot() +
  ggtitle("Boxplot of BMI by Residency") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Residency") +ylab("BMI") + labs(fill = "Residency")

###### Data Vis: Categorical Vars ################
library("RColorBrewer") # color palettes - http://www.sthda.com/english/wiki/colors-in-r
##### Gender #####
par(mar=c(3, 5, 3, 5))
x = barplot(prop.table(table(df.c$gender, df.c$rural), margin = 2),
            col = rep(c('plum1', 'lightskyblue1')),
            legend = TRUE,
            args.legend = list(x = "topright", inset = c(- 0.45, 0)),
            xlab = c("Residency"),
            ylim = c(0, 1),
            yaxt = 'n', # remove y-axis
            ylab = 'Percent of participants', 
            main = 'Gender Proportion by Residency')
# creating a y-axis with percentages
yticks = seq(0, 1, by = 0.10)
axis(2, at = yticks, lab = yticks)
# showing the values of each category
y = prop.table(table(df.c$gender, df.c$rural), margin = 2)
# 2 decimal places: sprintf(number, fmt = '%#.2f')
text(x, y[1,]/2, labels = paste0(as.character( sprintf(y[1,]*100, fmt = '%#.2f') ), '%')) # placing female values
text(x, y[1,] + y[2,]/2, labels = paste0( sprintf(y[2,]*100, fmt = '%#.2f') , '%')) # placing male values

##### Race #####
par(mar=c(3, 5, 3, 6))
x = barplot(prop.table(table(df.c$race, df.c$rural), margin = 2),
            col=brewer.pal(n = 5, name = "Pastel2"),
            legend = TRUE,
            args.legend = list(x = "topright", inset = c(- 0.65, 0)),
            xlab = c("Residency"),
            ylim = c(0, 1),
            yaxt = 'n', # remove y-axis
            ylab = 'Percent of participants', 
            main = 'Race Proportion by Residency')
# creating a y-axis with percentages
yticks = seq(0, 1, by = 0.10)
axis(2, at = yticks, lab = yticks)

##### Education #####
par(mar=c(3, 5, 3, 6))
x = barplot(prop.table(table(df.c$education, df.c$rural), margin = 2),
            col=brewer.pal(n = 7, name = "RdBu"),
            legend = TRUE,
            args.legend = list(x = "topright", inset = c(- 0.65, 0)),
            xlab = c("Residency"),
            ylim = c(0, 1),
            yaxt = 'n', # remove y-axis
            ylab = 'Percent of participants', 
            main = 'Education Proportion by Residency')
# creating a y-axis with percentages
yticks = seq(0, 1, by = 0.10)
axis(2, at = yticks, lab = yticks)

##### Smoke100 #####
par(mar=c(3, 5, 3, 6))
x = barplot(prop.table(table(df.c$smoke100, df.c$rural), margin = 2),
            col=brewer.pal(n = 7, name = "RdBu"),
            legend = TRUE,
            args.legend = list(x = "topright", inset = c(- 0.45, 0)),
            xlab = c("Residency"),
            ylim = c(0, 1),
            yaxt = 'n', # remove y-axis
            ylab = 'Percent of Participants', 
            main = 'Smoke100 Proportion by Residency')
# creating a y-axis with percentages
yticks = seq(0, 1, by = 0.10)
axis(2, at = yticks, lab = yticks)
# showing the values of each category
y = prop.table(table(df.c$smoke100, df.c$rural), margin = 2)
# 2 decimal places: sprintf(number, fmt = '%#.2f')
text(x, y[1,]/2, labels = paste0(as.character( sprintf(y[1,]*100, fmt = '%#.2f') ), '%')) # placing female values
text(x, y[1,] + y[2,]/2, labels = paste0( sprintf(y[2,]*100, fmt = '%#.2f') , '%')) # placing male values

##### Sleep Quality #####
par(mar=c(3, 5, 3, 6))
x = barplot(prop.table(table(df.c$sleepquality7d, df.c$rural), margin = 2),
            col=brewer.pal(n = 5, name = "PiYG"),
            legend = TRUE,
            args.legend = list(x = "topright", inset = c(- 0.65, 0)),
            xlab = c("Residency"),
            ylim = c(0, 1),
            yaxt = 'n', # remove y-axis
            ylab = 'Percent of Participants', 
            main = 'Sleep Quality Proportion by Residency')
# creating a y-axis with percentages
yticks = seq(0, 1, by = 0.10)
axis(2, at = yticks, lab = yticks)
D





