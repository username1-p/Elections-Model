# Elections-Model
This repo contains code for my GLM which was used to predict most the popular vote for the USA 2020 presidential elections. It was created by Prinsa Gandhi. The purpose is to create a report which will summarize the results of my generalized linear model. I have listed steps below to obtain the data I have used, as I am unable to share the data used in this model. The sections of this repo are output and scripts. The input section is not included in this repo as it is the data I used, however, I have included steps to obtain the data. 

Inputs contain data that are unchanged from their original. I used two datasets:
SURVEY DATA:
- The first one is Democracy Fund + UCLA Nationscape ‘Full Data Set’

(Tausanovitch, Chris and Lynn Vavreck. 2020. Democracy Fund + UCLA Nationscape,   October 10-17, 2019 (version 20200814). Retrieved from           [https://www.voterstudygroup.org/downloads?key=46a716b2-7321-4fcf-9ee6-8987a584a253].)

To obtain this data, first request access to the data by going to the following website:
https://www.voterstudygroup.org/publication/nationscape-data-set

An email will be sent to you with additional instructions to be able to download this data.

CENSUS DATA:
The second dataset is the 2018 American Community Surveys (ACS). 

(Steven Ruggles, Sarah Flood, Ronald Goeken, Josiah Grover, Erin Meyer, Jose     Pacas and Matthew Sobek. IPUMS USA: Version 10.0 [dataset][usa_00002.dta.gz].   Minneapolis, MN:IPUMS, 2020. https://doi.org/10.18128/D010.V10.0)

In order to obtain this data, the first step is to go to the following link and create an account with IPUMS:
https://usa.ipums.org/usa/index.shtml

I selected the 2018 ACS, and selected variables that I was interested in to use in post-stratification. The next step is to submit the request for the data extract,and download it when it is ready. 

My output files contains my report work and additional material that support my report. 

It contains the report.Rmd file, as well as my references list. 

The script file includes R scripts that I used to do important coding work before I moved onto the report. It contains methods I used to clean the data, as well as model the data. It contains scripts that produces outputs. 

Scripts contain R scripts that take inputs and outputs and produce outputs. 
It contains:
01-data_cleaning-post-strat1(1).R 
and 
01-data_Cleaning-survey1.R
modeling.R

