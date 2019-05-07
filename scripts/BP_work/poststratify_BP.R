###########
# Perform computation of post-stratification weights
###########

# Set up
rm(list = ls())
# install.packages("survey")
# install.packages('dplyr')
# install.packages('readr')

require(plyr); require(dplyr); require(readr); require(survey)
setwd("~/Desktop/Bhaven/Harvard/Classes/CS208/DPsurveyweighting/data/")
acs_cell_counts <- read.csv("cell_counts_3var.csv"); #get cell counts from file
acs_cell_counts <- acs_cell_counts[,-1]; #get rid of index column
cces16 <- read_tsv("CCES16_Common_OUTPUT_Jul2017_VV.tab", col_names = TRUE);

# process CCES data. Variables we're interested in. Some variables are commented out because we're not using them
weight_vars_cces <- c(#"gender", # sex
  #"birthyr", # age
  "educ", # education
  "race", # race
  #"employ", # employment status
  #"marstat", # marital status
  #"faminc", # family income
  #"child18num", # number of children under 18
  #"immstat", # citizenship status
  "inputstate" # state
  # metropolitan area
) 

#format CCES data to get rid of some weird categories
cces16 <- cces16 %>%
  mutate(education = case_when(educ %in% c(1,8,9,NA) ~ 1,
                          educ == 2 ~ 2,
                          educ == 3 ~ 3,
                          educ == 4 ~ 4,
                          educ == 5 ~ 5,
                          educ == 6 ~ 6),
         race = case_when(race == 1 ~ 1,
                          race == 2 ~ 2,
                          race == 3 ~ 3,
                          race == 4 ~ 4,
                          race %in% c(5,6,7,8,98,99,NA) ~ 5)
         )
states <- data.frame(inputstate = seq(1:56), 
                     state = tolower(c("AL","AK","","AZ","AR","CA","","CO","CT","DE","DC",
                                       "FL","GA","","HI","ID","IL","IN","IA","KS","KY","LA",
                                       "ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH",
                                       "NJ","NM","NY","NC","ND","OH","OK","OR","PA","","RI","SC",
                                       "SD","TN","TX","UT","VT","VA","","WA","WV","WI","WY")))
cces16 <- left_join(cces16, states, by = "inputstate")

cces16_slim <- cces16 %>% 
  mutate(all_vars = paste("state", state, "race", race, "education", education, sep="_")) %>% 
  select(all_vars, CC16_364c)

# process ACS data
acs_cell_counts_slim <- acs_cell_counts %>% 
  mutate(all_vars = paste("state", state, "race", race, "education", education, sep="_")) %>% 
  select(all_vars, n)

# create svydesign object
# if I use fpc = rep(sum(acs_cell_counts$n), nrow(cces16)), assume:
# (1) SRS for illustrative purposes
# (2) US population size = weighted sum from ACS --> 308532957, which may be off by 5-10 million

#ids = ~ 1 means no clusters, every person has same probability of being sampled
#pass in CCES data because that's the data we want weights for
cces16.des <- svydesign(ids = ~ 1, data = cces16_slim)
cces16.des.ps <- postStratify(design = cces16.des,
                              strata = ~all_vars,
                              population = acs_cell_counts_slim,
                              partial = TRUE)




acs_all_vars <- unique(acs_cell_counts_slim$all_vars);
cces_all_vars <- unique(cces16_slim$all_vars);

for(i in 1:length(cces_all_vars)){
  cces_var <- cces_all_vars[i];
  if(cces_var %in% acs_all_vars){
    next();
  }
  else{
    print(cces_var)
  }
}

