###########
# Perform computation of post-stratification weights
###########

# Set up
rm(list = ls())
setwd("~/Desktop/Harvard/S19/cs208/DPsurveyweighting")
# install.packages("survey")
require(plyr); require(dplyr); require(readr); require(survey)
source("scripts/dp_utils.R")
acs_cell_counts <- read.csv("data/cell_counts_3var.csv")
state_weights <- read.csv("data/state_weights.csv")
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

cces16_slim <- cces16 %>% 
  mutate(all_vars = paste("state", state, "race", race, "education", education, sep="_")) %>% 
  select(all_vars, CC16_364c)
  
# process ACS data
# rescale weights by diving by the max weight for any individual in any state
acs_cell_counts$rescaled_n <- acs_cell_counts$n/max(state_weights$max_weight)

acs_cell_counts_slim <- acs_cell_counts %>% 
  mutate(all_vars = paste("state", state, "race", race, "education", education, sep="_")) %>% 
  select(all_vars, rescaled_n)


# create svydesign object
# assume SRS for illustrative purposes
cces16.des <- svydesign(ids = ~ 1, data = cces16_slim)
cces16.des.ps <- postStratify(design = cces16.des,
                              strata = ~all_vars,
                              population = acs_cell_counts_slim,
                              partial = TRUE)

# 
voteshare.weighted <- as.data.frame(svytable(~CC16_364c, design=cces16.des.ps)) %>% 
  rename(preference=CC16_364c, share=Freq) %>% 
  mutate(preference = case_when(preference == 1 ~ "Trump",
                                preference == 2 ~ "Clinton",
                                preference == 3 ~ "Johnson",
                                preference == 4 ~ "Stein",
                                preference == 5 ~ "Other",
                                preference == 6 ~ "Won't vote",
                                preference == 7 ~ "Not sure",
                                preference %in% c(8,9, NA) ~ "Skipped/not asked"),
         share = share/sum(share))
voteshare.weighted

# unweighted counts
unweighted.res <- cces16 %>% rename(preference=CC16_364c) %>% 
  group_by(preference) %>% summarise(n = n()) %>% 
  mutate(preference = case_when(preference == 1 ~ "Trump",
                                preference == 2 ~ "Clinton",
                                preference == 3 ~ "Johnson",
                                preference == 4 ~ "Stein",
                                preference == 5 ~ "Other",
                                preference == 6 ~ "Won't vote",
                                preference == 7 ~ "Not sure",
                                preference %in% c(8,9, NA) ~ "Skipped/not asked"),
         share = n/sum(n)) %>% select(preference, share) 
unweighted.res


# Appendix
# compare to actual CCES weights
# cces.16.actual.des <- svydesign(ids = ~1, data = cces16, weights = ~commonweight)
# cces16.actual.svytable <- as.data.frame(svytable(~CC16_364c, design=cces.16.actual.des)) %>% 
#   rename(preference=CC16_364c, share=Freq) %>% 
#   mutate(preference = case_when(preference == 1 ~ "Trump",
#                                 preference == 2 ~ "Clinton",
#                                 preference == 3 ~ "Johnson",
#                                 preference == 4 ~ "Stein",
#                                 preference == 5 ~ "Other",
#                                 preference == 6 ~ "Won't vote",
#                                 preference == 7 ~ "Not sure",
#                                 preference %in% c(8,9) ~ "Skipped/not asked"),
#          share = share/sum(share))
# cces16.actual.svytable
