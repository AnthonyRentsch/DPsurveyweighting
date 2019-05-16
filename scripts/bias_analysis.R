###########
# Analyze differences in preferences between small and large cell counts
###########

# Set up
rm(list = ls())
setwd("~/Desktop/Harvard/S19/cs208/DPsurveyweighting")
# install.packages("survey")
require(plyr); require(dplyr); require(ggplot2); require(readr); require(survey)
source("scripts/dp_utils.R")
acs_cell_counts <- read.csv("data/acs_cell_counts_clean.csv")
acs_cell_counts <- acs_cell_counts[,-1]
state_weights <- read.csv("data/state_weights.csv")
cces16 <- read_tsv("data/CCES16_Common_OUTPUT_Jul2017_VV.tab", col_names = TRUE)

# Process CCES data
weight_vars_cces <- c("gender", # sex
                      "birthyr", # age
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
                          race %in% c(5,6,7,8,98,99,NA) ~ 5),
         sex = gender,
         age = case_when(2016-birthyr < 35 | is.na(birthyr) ~ 1,
                         2016-birthyr >= 35 & 2016-birthyr < 50 ~ 2,
                         2016-birthyr >= 50 & 2016-birthyr < 65 ~ 3,
                         2016-birthyr >= 65 ~ 4)
  )
states <- data.frame(inputstate = seq(1:56), 
                     state = tolower(c("AL","AK","","AZ","AR","CA","","CO","CT","DE","DC",
                                       "FL","GA","","HI","ID","IL","IN","IA","KS","KY","LA",
                                       "ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH",
                                       "NJ","NM","NY","NC","ND","OH","OK","OR","PA","","RI","SC",
                                       "SD","TN","TX","UT","VT","VA","","WA","WV","WI","WY")))
cces16 <- left_join(cces16, states, by = "inputstate")

cces16_slim <- cces16 %>% 
  mutate(preference = case_when(CC16_364c == 1 ~ "Trump",
                                CC16_364c == 2 ~ "Clinton",
                                CC16_364c == 3 ~ "Johnson",
                                CC16_364c == 4 ~ "Stein",
                                CC16_364c == 5 ~ "Other",
                                CC16_364c == 6 ~ "Won't vote",
                                CC16_364c %in% c(7,8,9) | is.na(CC16_364c) ~ "No response"),
         assault_rifle_ban = case_when(CC16_330d == 1 ~ "Support",
                                       CC16_330d == 2 ~ "Oppose",
                                       !CC16_330d %in% c(1,2) ~ "No response")
  ) %>% 
  select(state, education, race, sex, age, preference, assault_rifle_ban) 

# Process ACS data
acs_cell_counts$n <- ifelse(acs_cell_counts$n < 1, 1, acs_cell_counts$n)
acs_cell_counts_slim <- acs_cell_counts %>% select(state, education, race, sex, age, n)

# Merge CCES and ACS
acs_cces_merged <- left_join(cces16_slim, acs_cell_counts_slim, 
                             by=c("state","sex","race","age","education"),
                             suffixes=c("_cces","_acs"))

# Clinton lead x race
acs_cces_merged %>% group_by(n < 10000, race, preference) %>% 
  summarise(n = n()) %>% mutate(share = n/sum(n)) %>% 
  filter(preference %in% c("Clinton","Trump")) %>% View()

# Clinton lead x education
acs_cces_merged %>% group_by(n < 10000, education, preference) %>% 
  summarise(n = n()) %>% mutate(share = n/sum(n)) %>% 
  filter(preference %in% c("Clinton","Trump")) %>% View()

# Support for assault rifle ban x race
acs_cces_merged %>% group_by(n < 10000, race, assault_rifle_ban) %>% 
  summarise(n = n()) %>% mutate(share = n/sum(n)) %>% 
  filter(preference %in% c("Clinton","Trump")) %>% View()

# Support for assault rifle ban x education
acs_cces_merged %>% group_by(n < 10000, education, assault_rifle_ban) %>% 
  summarise(n = n()) %>% mutate(share = n/sum(n)) %>% 
  filter(preference %in% c("Clinton","Trump")) %>% View()
