rm(list = ls())
library(tidyverse)
library(nnet)
library(rms)
library(glmnet)
library(ggplot2)
library(rms)

setwd("E:/sta304/ps3")
datafile <- read.csv("survey_datafinal2.csv", header=TRUE)
datafile <- as.data.frame(datafile)
is.na(datafile)
colSums(is.na(datafile))


#omit missing values
datafile <- na.omit(datafile)

#full model
logmodel<-glm(formula = vote_trump ~ . - vote_2020 - age 
                    
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
                      census_region+race+hispanic+agegrp+household_income+
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
                      census_region+race+hispanic+agegrp
                      
                    
                    , data=datafile, family="binomial")
summary(logmodelselect2)


logmodelselect3<-glm(formula = vote_trump ~ state+ education+ foreign_born+gender+
                       census_region+race+hispanic+agegrp
                     
                     
                     , data=datafile, family="binomial")

#census region is not as significant, use state instead it is very significant
logmodelselect4<-glm(formula = vote_trump ~ state+ education+ foreign_born+gender+
                       Race+hispanic+agegrp
                     
                     
                     , data=datafile, family="binomial")
anova(logmodelselect3,logmodelselect4, test="Chisq")
summary(logmodelselect4)


#biden

logmodelselectb<-glm(formula = vote_biden ~ state+ education+ foreign_born+gender+
                       Race+hispanic+agegrp
                     
                     
                     , data=datafile, family="binomial")
anova(logmodelselect3,logmodelselect4, test="Chisq")
summary(logmodelselect4)
logmodelselecttry<-glm(formula = vote_trump ~  foreign_born+gender+
                       Race+hispanic+agegrp+education
                     
                     
                     , data=datafile, family="binomial")


census_data <- read_csv("census_datareduced2.csv")

census_data <- census_data[!(census_data$agegrp=="0-10"),]
census_data <- census_data[!(census_data$agegrp=="11-17"),]
census_data <- census_data[!(census_data$agegrp=="95+"),]
# Here I will perform the post-stratification calculation
census_data$estimate <-
  logmodelselect4 %>%
  predict(newdata = census_data, type="response")

census_data$vote <- ifelse(census_data$estimate > 0.5, "1", "0")
census_data$vote<- as.numeric(census_data$vote)
mean(census_data$vote)



census_data %>%
  mutate(alp_predict_prop = estimate*n) %>%
  summarise(alp_predict = sum(alp_predict_prop)/sum(n))



census_data %>%
  mutate(alp_predict_prop = vote*n) %>%
  summarise(alp_predict = sum(alp_predict_prop)/sum(n))


census_data$estimate <-
  logmodelselecttry %>%
  predict(newdata = census_data, type="response")

census_data %>%
  mutate(alp_predict_prop = estimate*n) %>%
  summarise(alp_predict = sum(alp_predict_prop)/sum(n))


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
