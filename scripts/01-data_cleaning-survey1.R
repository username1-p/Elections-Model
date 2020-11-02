#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("E:/sta304/ps3")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("ns20200625/ns20200625.dta")

# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- 
  raw_data %>% 
 
  

select(vote_2020,
       employment,
       foreign_born,
       age,
       gender,
       #sex,
       census_region,
       race_ethnicity, 
       hispanic,
       household_income,
       education,
       state,
       congress_district
       
       
       
       
  
  
)


#### What else???? ####
# Maybe make some age-groups?
# Maybe check the values?
# Is vote a binary? If not, what are you going to do?

reduced_data<-
  reduced_data %>%
  mutate(vote_trump = 
           ifelse(vote_2020=="Donald Trump", 1, 0))


reduced_data<-
  reduced_data %>%
  mutate(vote_biden = 
           ifelse(vote_2020=="Joe Biden", 1, 0))

reduced_data<-
  reduced_data %>%
  mutate(foreign_born = 
           ifelse(foreign_born=="The United States", 1, 0))
# 1 USA, 0 other country

#making age groups
reduced_data %>% 
  add_column(new_col = "agegrp")

reduced_data$agegrp[reduced_data$age <=10] <- "0-10"
reduced_data$agegrp[11<= reduced_data$age & reduced_data$age<= 17] <- "11-17"
reduced_data$agegrp[18<= reduced_data$age & reduced_data$age<= 25] <- "18-25"
reduced_data$agegrp[26<= reduced_data$age & reduced_data$age<= 35] <- "26-35"
reduced_data$agegrp[36<= reduced_data$age & reduced_data$age<= 45] <- "36-45"
reduced_data$agegrp[46<= reduced_data$age & reduced_data$age<= 55] <- "46-55"
reduced_data$agegrp[56<= reduced_data$age & reduced_data$age<= 65] <- "56-65"
reduced_data$agegrp[66<= reduced_data$age & reduced_data$age<= 75] <- "66-75"
reduced_data$agegrp[76<= reduced_data$age & reduced_data$age<= 85] <- "76-85"
reduced_data$agegrp[86<= reduced_data$age & reduced_data$age<= 95] <- "86-95"
reduced_data$agegrp[reduced_data$age >=95] <- "95+"


reduced_data<-
  reduced_data %>%
  mutate(hispanic = 
           ifelse(hispanic=="Not Hispanic", 0, 1))

#not hispanic is 0, all else is 1

#race fix for census
reduced_data %>% 
  add_column(new_col = "Race")


reduced_data$Race[  reduced_data$race_ethnicity == "Asian (Filipino)" ] <-  "other asian or pacific islander"
reduced_data$Race[reduced_data$race_ethnicity == "Asian (Asian Indian)"]<-  "other asian or pacific islander"
reduced_data$Race[reduced_data$race_ethnicity == "Asian (Korean)"]<-  "other asian or pacific islander"
reduced_data$Race[reduced_data$race_ethnicity == "Asian (Other)"] <- "other asian or pacific islander"
                              
reduced_data$Race[reduced_data$race_ethnicity == "Asian (Vietnamese)"] <- "other asian or pacific islander"
reduced_data$Race[reduced_data$race_ethnicity == "Pacific Islander (Guamanian)"] <-  "other asian or pacific islander"
reduced_data$Race[ reduced_data$race_ethnicity == "Pacific Islander (Native Hawaiian)"]<- "other asian or pacific islander"
reduced_data$Race[reduced_data$race_ethnicity == "Pacific Islander (Other)"]<- "other asian or pacific islander"
reduced_data$Race[reduced_data$race_ethnicity == "Pacific Islander (Samoan)"]<- "other asian or pacific islander"
                              
reduced_data$Race[reduced_data$race_ethnicity == "Pacific Islander (Samoan)"] <- "other asian or pacific islander"

reduced_data$Race[reduced_data$race_ethnicity == "American Indian or Alaska Native"] <- "American Indian or Alaska Native"

reduced_data$Race[reduced_data$race_ethnicity == "Asian (Chinese)"] <- "Asian (Chinese)"
reduced_data$Race[reduced_data$race_ethnicity == "Asian (Japanese)"] <- "Asian (Japanese)"

reduced_data$Race[reduced_data$race_ethnicity == "Black, or African American"] <- "Black, or African American"
reduced_data$Race[reduced_data$race_ethnicity == "Some other race"] <- "Some other race"

reduced_data$Race[reduced_data$race_ethnicity == "White"] <- "White"





# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_data, "survey_datafinal2.csv")


