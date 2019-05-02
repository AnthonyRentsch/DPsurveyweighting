###########
# Perform computation of post-stratification weights
###########

# Set up
rm(list = ls())
# install.packages("survey")
require(plyr); require(dplyr); require(readr); require(survey)
setwd("~/Desktop/Harvard/S19/cs208/DPsurveyweighting/")
acs_cell_counts <- read.csv("data/cell_counts.csv")
acs_cell_counts <- acs_cell_counts[,-1]
cces16 <- read_tsv("data/CCES16_Common_OUTPUT_Jul2017_VV.tab", col_names = TRUE)

# process CCES data
weight_vars_cces <- c(#"gender", # sex
                      #"birthyr", # age
                      "educ", # education
                      "race", # race
                      #"employ", # employment status
                      #"marstat", # marital status
                      #"faminc", # family income
                      #"child18num", # number of children under 18
                      #"immstat", # citizenship status
                      "input_state" # state
                      # metropolitan area
                      ) 

cces16 <- cces16 %>% rename(state = input_state) %>% 
  mutate(education = case_when(educ == c(1,8,9,NA) ~ 1,
                          educ == 2 ~ 2
                          educ == 3 ~ 3
                          educ == 4 ~ 4
                          educ == 5 ~ 5
                          educ == 6 ~ 6),
         race = case_when(race == 1 ~ 1,
                          race == 2 ~ 2,
                          race == 3 ~ 3,
                          race == 4 ~ 4,
                          race %in% c(5,6,7,8,98,99,NA) ~ 5)
         )




?postStratify
