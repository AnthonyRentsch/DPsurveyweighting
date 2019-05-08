###########
# Perform computation of post-stratification weights
# this is the second version of this code after Anthony pulled the new ACS data (after fixing bug with DC).
# post-stratification weights work now
###########

# Set up
rm(list = ls())
# install.packages("survey")
require(plyr); require(dplyr); require(readr); require(survey)
setwd("~/Desktop/Bhaven/Harvard/Classes/CS208/DPsurveyweighting/data/")
acs_cell_counts <- read.csv("cell_counts_3var.csv")
acs_cell_counts <- acs_cell_counts[,-1]
cces16 <- read_tsv("CCES16_Common_OUTPUT_Jul2017_VV.tab", col_names = TRUE)

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
                      "inputstate" # state
                      # metropolitan area
                      ) 

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

# cces16_slim <- cces16 %>% 
#   mutate(all_vars = paste("state", state, "race", race, "education", education, sep="_")) %>% 
#   select(all_vars, CC16_364c)
  
# process ACS data
# acs_cell_counts_slim <- acs_cell_counts %>% 
#   mutate(all_vars = paste("state", state, "race", race, "education", education, sep="_")) %>% 
#   select(all_vars, n)
acs_cell_counts_slim <- acs_cell_counts[, c('state', 'n','race', 'education' )]



# create svydesign object
# assume: SRS for illustrative purposes
# if I use fpc = rep(sum(acs_cell_counts$n), nrow(cces16)), assume:
# US population size = weighted sum from ACS --> 308532957, which may be off by 5-10 million

# cces16.des <- svydesign(ids = ~ 1, data = cces16_slim)
cces16.des <- svydesign(ids = ~ 1, data = cces16)
cces16.des.ps <- postStratify(design = cces16.des,
                              strata = ~state+race+education,
                              population = acs_cell_counts_slim,
                              partial = TRUE)

# test out results
#### these give different answers so they are not equivalent ####
#### I'm guessing 1st way is right ####
#### ex: https://stats.idre.ucla.edu/r/faq/how-do-i-analyze-survey-data-with-stratification-after-sampling-poststratification/ ####
#### test by making a svydesign object using just the 2016 CCES and computing same values ####

ps.res <- as.data.frame(svytable(~CC16_364c, design=cces16.des.ps)) %>% #create a survey table
  rename(preference=CC16_364c, share=Freq) %>% #
  mutate(preference = case_when(preference == 1 ~ "Trump",
                                preference == 2 ~ "Clinton",
                                preference == 3 ~ "Johnson",
                                preference == 4 ~ "Stein",
                                preference == 5 ~ "Other",
                                preference == 6 ~ "Won't vote",
                                preference == 7 ~ "Not sure",
                                preference %in% c(8,9) ~ "Skipped/not asked"),
         share = share/sum(share))
ps.res

#
cces16.w.weights <- as.data.frame(cces16_slim) 
cces16.w.weights$weight <- cces16.des.ps$postStrata[[1]]; #add weights to the dataframe
cces16.w.weights %>% rename(preference=CC16_364c) %>% #rename the column to preference
  group_by(preference) %>% summarise(n = sum(weight)) %>%  #groupby preference and sum the weights for each preference
  mutate(preference = case_when(preference == 1 ~ "Trump", #change preferences to 
                                preference == 2 ~ "Clinton",
                                preference == 3 ~ "Johnson",
                                preference == 4 ~ "Stein",
                                preference == 5 ~ "Other",
                                preference == 6 ~ "Won't vote",
                                preference == 7 ~ "Not sure",
                                preference %in% c(8,9) ~ "Skipped/not asked"),
         share = n/sum(n)) %>% select(preference, share)

# compare to actual CCES weights



