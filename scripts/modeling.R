rm(list = ls())
library(tidyverse)
library(glmnet)
library(ggplot2)


setwd("E:/sta304/ps3")
datafile <- read.csv("survey_datafinal2.csv", header=TRUE)
datafile <- as.data.frame(datafile)
is.na(datafile)
colSums(is.na(datafile))


#omit missing values
datafile <- na.omit(datafile)

#full model
logmodel<-glm(formula = vote_trump ~ . - vote_2020 - agegrp 
                    
                    , data=datafile, family="binomial")
summary(logmodel)


#aic
## Stepwise elimination based on AIC ##
aic <- step(logmodel, trace = 1, k=2)


# BIC
n <- nrow(datafile)
sel.var.bic <- step(logmodel, trace = 0, k = log(n), direction = "both") 
sel.var.bic<-attr(terms(sel.var.bic), "term.labels")   
sel.var.bic

logmodelselect<-glm(formula = vote_trump ~ education + foreign_born+gender+
                      census_region+Race+hispanic+agegrp+household_income+
                      employment
              
              , data=datafile, family="binomial")
summary(logmodelselect)

#aic
## Stepwise elimination based on AIC ##
aic <- step(logmodelselect, trace = 1, k=2)
      

# BIC
n <- nrow(datafile)
sel.var.bic <- step(logmodelselect, trace = 0, k = log(n), direction = "both") 
sel.var.bic<-attr(terms(sel.var.bic), "term.labels")   
sel.var.bic


logmodelselect2<-glm(formula = vote_trump ~ education + foreign_born+gender+
                      census_region+Race+hispanic+agegrp
                      
                    
                    , data=datafile, family="binomial")
summary(logmodelselect2)


logmodelselect3<-glm(formula = vote_trump ~ state+ education+ foreign_born+gender+
                       census_region+Race+hispanic+agegrp
                     
                     
                     , data=datafile, family="binomial")

#census region is not as significant, use state instead it is very significant

#trump model:
logmodelselect4<-glm(formula = vote_trump ~ state+ education+ foreign_born+gender+
                       Race+hispanic+agegrp
                     
                     
                     , data=datafile, family="binomial")
anova(logmodelselect3,logmodelselect4, test="Chisq")
summary(logmodelselect4)


aic <- step(logmodelselect4, trace = 1, k=2)
#biden

logmodelselectb<-glm(formula = vote_biden ~ state+ education+ foreign_born+gender+
                       Race+hispanic+agegrp
                     
                     
                     , data=datafile, family="binomial")

anova(logmodelselect3,logmodelselect4, test="Chisq")
summary(logmodelselect4)
#aside
logmodelselecttry<-glm(formula = vote_trump ~  foreign_born+gender+
                       Race+hispanic+agegrp+education
                     
                     
                     , data=datafile, family="binomial")


census_data <- read_csv("census_datareduced3.csv")



# Here I will perform the post-stratification calculation
#take away new factors
census_data <- census_data[!(census_data$agegrp=="0-10"),]
census_data <- census_data[!(census_data$agegrp=="11-17"),]
census_data <- census_data[!(census_data$agegrp=="95+"),]
census_data <- census_data[!(census_data$state=="Other"),]

#trump

census_data$logodds_estimate <-
  logmodelselect4 %>%
  predict(newdata = census_data)

census_data$estimate <-
  exp(census_data$logodds_estimate)/(1+exp(census_data$logodds_estimate))

estimatetrump <- census_data %>%
  mutate(alp_predict_prop = estimate*n) %>%
  summarise(alp_predict = sum(alp_predict_prop)/sum(n))

#biden
census_data$logodds_estimate <-
  logmodelselectb %>%
  predict(newdata = census_data)

census_data$estimate <-
  exp(census_data$logodds_estimate)/(1+exp(census_data$logodds_estimate))

estimatebiden <-  census_data %>%
  mutate(alp_predict_prop = estimate*n) %>%
  summarise(alp_predict = sum(alp_predict_prop)/sum(n))

estimatetrump
estimatebiden


#reload survey data 

survey_data <- read.csv("survey_datafinal2.csv", header=TRUE)

#plots:
  survey_data %>% 
  ggplot(aes(y=vote_trump, x = education)) +
  geom_boxplot() +ggtitle("Education vs Vote for Trump") +labs(y= "Vote for Trump", x = "Education")
  
length(which(survey_data$vote_2020=="I am not sure/don't know"))

  

survey_data %>% 
  ggplot(aes(y=vote_trump, x = Race)) +
  geom_boxplot() +ggtitle("Race vs Vote for Trump")

survey_data %>% 
  ggplot(aes(y=vote_2020, x = age)) +
  geom_boxplot() +ggtitle("Age vs Vote 2020")

