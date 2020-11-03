#### Preamble ####
# Purpose: The purpose is to clean the data obtained from the IPUMS website
#and prepare it so that it can be used as part of my model and match it to the survey data
# Author: Prinsa Gandhi
# Data: 02 November 2020
# Contact: prinsa.gandhi@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore 


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.


#setwd manually
# Read in the raw data (You might need to change this if you use a different dataset)
datacen <- read_dta("usa_00002.dta.gz")


# Add the labels
datacen <- labelled::to_factor(datacen)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
datacenreduced <- 
  datacen %>% 
  select(
        educ,
        sex,
        region,
        race,
        hispan,
        age,
        bpl,stateicp)
        
        
        
#matching variable values to original  dataset
#vote_trump ~   
#  +state


#race has missing values, fix it up



#1) education

datacenreduced <- as.data.frame(datacenreduced)
datacenreduced %>% 
  add_column(new_col = "education")


datacenreduced$education[datacenreduced$educ =="1 year of college"] <- "Completed some college, but no degree"
datacenreduced$education[datacenreduced$educ =="2 years of college"] <- "Completed some college, but no degree"
datacenreduced$education[datacenreduced$educ =="4 years of college"] <- "College Degree (such as B.A., B.S.)"  
datacenreduced$education[datacenreduced$educ =="5+ years of college"] <- "College Degree (such as B.A., B.S.)"
datacenreduced$education[datacenreduced$educ =="grade 10"] <- "Completed some high school"
datacenreduced$education[datacenreduced$educ =="grade 11"] <- "Completed some high school"
datacenreduced$education[datacenreduced$educ =="grade 12"] <- "High school graduate"
datacenreduced$education[datacenreduced$educ =="grade 5, 6, 7, or 8"] <- "Middle School - Grades 4 - 8"
datacenreduced$education[datacenreduced$educ =="grade 9"] <- "Completed some high school"
datacenreduced$education[datacenreduced$educ =="n/a or no schooling"] <- "3rd Grade or less"
datacenreduced$education[datacenreduced$educ =="nursery school to grade 4"] <- "3rd Grade or less"

datacenreduced <- datacenreduced[ -c(1) ]

#sex to gender



datacenreduced %>% 
  add_column(new_col = "gender")


datacenreduced$gender[datacenreduced$sex =="female"] <- "Female"
datacenreduced$gender[datacenreduced$sex =="male"] <- "Male"

datacenreduced <-  datacenreduced[-c(1)]


#not using census region
datacenreduced <- datacenreduced[-c(1)]

#checking na

colSums(is.na(datacenreduced))


#race
datacenreduced %>% 
  add_column(new_col = "Race")


datacenreduced$Race[datacenreduced$race =="american indian or alaska native"] <- "American Indian or Alaska Native"
datacenreduced$Race[datacenreduced$race =="black/african american/negro"] <- "Black, or African American"
datacenreduced$Race[datacenreduced$race =="chinese"] <- "Asian (Chinese)"

datacenreduced$Race[datacenreduced$race =="japanese"] <- "Asian (Japanese)"

datacenreduced$Race[datacenreduced$race =="other asian or pacific islander"] <- "other asian or pacific islander"
datacenreduced$Race[datacenreduced$race =="other race, nec"] <- "Some other race"
datacenreduced$Race[datacenreduced$race =="three or more major races"] <- "Some other race"
datacenreduced$Race[datacenreduced$race =="white"] <- "White"
datacenreduced$Race[datacenreduced$race =="two major races"] <- "Some other race"


datacenreduced <- datacenreduced[-c(1)]


#hispanic
datacenreduced %>% 
  add_column(new_col = "hispanic")

datacenreduced<-
  datacenreduced %>%
  mutate(hispanic = 
           ifelse(hispan=="not hispanic", 0, 1))

datacenreduced <- datacenreduced[-c(1)]

#age 
#making age groups

datacenreduced$age <- as.numeric(datacenreduced$age)

datacenreduced %>% 
  add_column(new_col = "agegrp")

datacenreduced$agegrp[datacenreduced$age <=10] <- "0-10"
datacenreduced$agegrp[11<= datacenreduced$age & datacenreduced$age<= 17] <- "11-17"

datacenreduced$agegrp[18<= datacenreduced$age & datacenreduced$age<= 25] <- "18-25"
datacenreduced$agegrp[26<= datacenreduced$age & datacenreduced$age<= 35] <- "26-35"
datacenreduced$agegrp[36<= datacenreduced$age & datacenreduced$age<= 45] <- "36-45"
datacenreduced$agegrp[46<= datacenreduced$age & datacenreduced$age<= 55] <- "46-55"
datacenreduced$agegrp[56<= datacenreduced$age & datacenreduced$age<= 65] <- "56-65"
datacenreduced$agegrp[66<= datacenreduced$age & datacenreduced$age<= 75] <- "66-75"
datacenreduced$agegrp[76<= datacenreduced$age & datacenreduced$age<= 85] <- "76-85"
datacenreduced$agegrp[86<= datacenreduced$age & datacenreduced$age<= 95] <- "86-95"
datacenreduced$agegrp[datacenreduced$age >=95] <- "95+"



#already converted to integer, so below doesnt work?

datacenreduced <- 
  datacenreduced %>% 
  filter(age != "less than 1 year old") %>%
  filter(age != "90 (90+ in 1980 and 1990)")

datacenreduced <- datacenreduced[-c(1)]


#foreign born variable
datacenreduced %>% 
  add_column(new_col = "foreign_born")
datacenreduced$foreign_born[datacenreduced$bpl =="alabama"] <- 1
datacenreduced$foreign_born[datacenreduced$bpl =="alaska"] <- 1
datacenreduced$foreign_born[datacenreduced$bpl =="american samoa"] <- 1

datacenreduced$foreign_born[datacenreduced$bpl =="americas, n.s"] <- 0
datacenreduced$foreign_born[datacenreduced$bpl =="arizona"] <- 1

datacenreduced$foreign_born[datacenreduced$bpl =="arkansas"] <- 1
datacenreduced$foreign_born[datacenreduced$bpl =="california"] <- 1

datacenreduced$foreign_born[datacenreduced$bpl =="colorado"] <- 1

datacenreduced$foreign_born[datacenreduced$bpl =="connecticut"] <- 1
datacenreduced$foreign_born[datacenreduced$bpl =="delaware"] <- 1
datacenreduced$foreign_born[datacenreduced$bpl =="district of columbia"] <- 1

datacenreduced$foreign_born[datacenreduced$bpl =="florida"] <- 1
datacenreduced$foreign_born[datacenreduced$bpl =="georgia"] <- 1
datacenreduced$foreign_born[datacenreduced$bpl =="hawaii"] <- 1
datacenreduced$foreign_born[datacenreduced$bpl =="idaho"] <- 1

datacenreduced$foreign_born[datacenreduced$bpl =="illinois"] <- 1

datacenreduced$foreign_born[datacenreduced$bpl =="indiana"] <- 1
datacenreduced$foreign_born[datacenreduced$bpl =="iowa"] <- 1

datacenreduced$foreign_born[datacenreduced$bpl =="kansas"] <- 1

datacenreduced$foreign_born[datacenreduced$bpl =="kentucky"] <- 1
datacenreduced$foreign_born[datacenreduced$bpl =="louisiana"] <- 1

datacenreduced$foreign_born[datacenreduced$bpl =="maine"] <- 1
datacenreduced$foreign_born[datacenreduced$bpl =="maryland"] <- 1
datacenreduced$foreign_born[datacenreduced$bpl =="massachusetts"] <- 1

datacenreduced$foreign_born[datacenreduced$bpl =="michigan"] <- 1
datacenreduced$foreign_born[datacenreduced$bpl =="minnesota"] <- 1

datacenreduced$foreign_born[datacenreduced$bpl =="mississippi"] <- 1

datacenreduced$foreign_born[datacenreduced$bpl =="missouri"] <- 1

datacenreduced$foreign_born[datacenreduced$bpl =="montana"] <- 1
datacenreduced$foreign_born[datacenreduced$bpl =="nebraska"] <- 1
datacenreduced$foreign_born[datacenreduced$bpl =="nevada"] <- 1

datacenreduced$foreign_born[datacenreduced$bpl =="new hampshire"] <- 1

datacenreduced$foreign_born[datacenreduced$bpl =="new jersey"] <- 1
datacenreduced$foreign_born[datacenreduced$bpl =="new york"] <- 1

datacenreduced$foreign_born[datacenreduced$bpl =="north carolina"] <- 1

datacenreduced$foreign_born[datacenreduced$bpl =="north dakota"] <- 1

datacenreduced$foreign_born[datacenreduced$bpl =="ohio"] <- 1
datacenreduced$foreign_born[datacenreduced$bpl =="oklahoma"] <- 1

datacenreduced$foreign_born[datacenreduced$bpl =="oregon"] <- 1
datacenreduced$foreign_born[datacenreduced$bpl =="pennsylvania"] <- 1

datacenreduced$foreign_born[datacenreduced$bpl =="rhode island"] <- 1

datacenreduced$foreign_born[datacenreduced$bpl =="south carolina"] <- 1
datacenreduced$foreign_born[datacenreduced$bpl =="south dakota"] <- 1

datacenreduced$foreign_born[datacenreduced$bpl =="tennessee"] <- 1
datacenreduced$foreign_born[datacenreduced$bpl =="texas"] <- 1

datacenreduced$foreign_born[datacenreduced$bpl =="u.s. virgin islands"] <- 1
datacenreduced$foreign_born[datacenreduced$bpl =="utah"] <- 1
datacenreduced$foreign_born[datacenreduced$bpl =="vermont"] <- 1
datacenreduced$foreign_born[datacenreduced$bpl =="virginia"] <- 1
datacenreduced$foreign_born[datacenreduced$bpl =="washington"] <- 1


datacenreduced$foreign_born[datacenreduced$bpl =="west virginia"] <- 1
datacenreduced$foreign_born[datacenreduced$bpl =="wisconsin"] <- 1

datacenreduced$foreign_born[datacenreduced$bpl =="wyoming"] <- 1

datacenreduced$foreign_born[is.na(datacenreduced$foreign_born)]<-0
colSums(is.na(datacenreduced))


datacenreduced <- datacenreduced[-c(1)]

#state
datacenreduced[,"state"] <- NA


datacenreduced$state[datacenreduced$stateicp =="alaska"] <- "AK"
datacenreduced$state[datacenreduced$stateicp =="alabama"] <- "AL"

datacenreduced$state[datacenreduced$stateicp =="arizona"] <- "AZ"

datacenreduced$state[datacenreduced$stateicp =="california"] <- "CA"
datacenreduced$state[datacenreduced$stateicp =="colorado"] <- "CO"
datacenreduced$state[datacenreduced$stateicp =="connecticut"] <- "CT"
datacenreduced$state[datacenreduced$stateicp =="district of columbia"] <- "DC"

datacenreduced$state[datacenreduced$stateicp =="delaware"] <- "DE"

datacenreduced$state[datacenreduced$stateicp =="florida"] <- "FL"
datacenreduced$state[datacenreduced$stateicp =="idaho"] <- "ID"

datacenreduced$state[datacenreduced$stateicp =="idaho"] <- "ID"

datacenreduced$state[datacenreduced$stateicp =="illinois"] <- "IL"
datacenreduced$state[datacenreduced$stateicp =="indiana"] <- "IN"
datacenreduced$state[datacenreduced$stateicp =="kansas"] <- "KS"

datacenreduced$state[datacenreduced$stateicp =="kentucky"] <- "KY"
datacenreduced$state[datacenreduced$stateicp =="louisiana"] <- "LA"
datacenreduced$state[datacenreduced$stateicp =="massachusetts"] <- "MA"

datacenreduced$state[datacenreduced$stateicp =="maryland"] <- "MD"
datacenreduced$state[datacenreduced$stateicp =="maine"] <- "ME"
datacenreduced$state[datacenreduced$stateicp =="michigan"] <- "MI"

datacenreduced$state[datacenreduced$stateicp =="minnesota"] <- "MN"

datacenreduced$state[datacenreduced$stateicp =="missouri"] <- "MO"
datacenreduced$state[datacenreduced$stateicp =="mississippi"] <- "MS"

datacenreduced$state[datacenreduced$stateicp =="montana"] <- "MT"
datacenreduced$state[datacenreduced$stateicp =="north carolina"] <- "NC"
datacenreduced$state[datacenreduced$stateicp =="north dakota"] <- "ND"
datacenreduced$state[datacenreduced$stateicp =="nebraska"] <- "NE"

datacenreduced$state[datacenreduced$stateicp =="new hampshire"] <- "NH"
datacenreduced$state[datacenreduced$stateicp =="new jersey"] <- "NJ"
datacenreduced$state[datacenreduced$stateicp =="new mexico"] <- "NM"

datacenreduced$state[datacenreduced$stateicp =="nevada"] <- "NV"
datacenreduced$state[datacenreduced$stateicp =="new york"] <- "NY"
datacenreduced$state[datacenreduced$stateicp =="ohio"] <- "OH"
datacenreduced$state[datacenreduced$stateicp =="oklahoma"] <- "OK"

datacenreduced$state[datacenreduced$stateicp =="oregon"] <- "OR"

datacenreduced$state[datacenreduced$stateicp =="pennsylvania"] <- "PA"
datacenreduced$state[datacenreduced$stateicp =="rhode island"] <- "RI"
datacenreduced$state[datacenreduced$stateicp =="south carolina"] <- "SC"
datacenreduced$state[datacenreduced$stateicp =="south dakota"] <- "SD"

datacenreduced$state[datacenreduced$stateicp =="tennessee"] <- "TN"
datacenreduced$state[datacenreduced$stateicp =="texas"] <- "TX"
datacenreduced$state[datacenreduced$stateicp =="utah"] <- "UT"

datacenreduced$state[datacenreduced$stateicp =="virginia"] <- "VA"
datacenreduced$state[datacenreduced$stateicp =="vermont"] <- "VT"
datacenreduced$state[datacenreduced$stateicp =="washington"] <- "WA"
datacenreduced$state[datacenreduced$stateicp =="wisconsin"] <- "WI"
datacenreduced$state[datacenreduced$stateicp =="west virginia"] <- "WV"
datacenreduced$state[datacenreduced$stateicp =="wyoming"] <- "WY"

datacenreduced$state[is.na(datacenreduced$state)]<-"Other"
#other states werent in survey data, no factor for these
colSums(is.na(datacenreduced))
datacenreduced <- datacenreduced[-c(1)]



#### What's next? ####

## Here I am only splitting cells by age, but you 
## can use other variables to split by changing
## count(age) to count(age, sex, ....)

datacenreduced2 <- 
  datacenreduced %>%
  count(agegrp, gender,  state, Race, hispanic, foreign_born, education) %>%
  group_by(agegrp, gender, state) 



#check file name! 

# Saving the census data as a csv file in my
# working directory
write_csv(datacenreduced2, "census_datafinal.csv")

