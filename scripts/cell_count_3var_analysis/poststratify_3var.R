###########
# Perform computation of post-stratification weights
###########

#### Set up
rm(list = ls());
setwd("../../"); #move working directory to the root of our DPsurveyweighting repo
# install.packages("survey")
require(plyr); require(dplyr); require(readr); require(survey)
source("scripts/dp_utils.R")
acs_cell_counts <- read.csv("data/cell_counts_3var.csv")
state_weights <- read.csv("data/state_weights.csv")
acs_cell_counts <- acs_cell_counts[,-1]
cces16 <- read_tsv("data/CCES16_Common_OUTPUT_Jul2017_VV.tab", col_names = TRUE)

# process CCES data
weight_vars_cces <- c("gender", # sex
                      "birthyr", # age
                      "educ", # education
                      "race", # race
                      "employ", # employment status
                      "marstat", # marital status
                      #"faminc", # family income
                      #"child18num", # number of children under 18
                      "immstat", # citizenship status
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
         age = case_when(2016-birthyr < 18 | is.na(birthyr) ~ 1,
                         2016-birthyr >= 18 | 2016-birthyr < 28 ~ 2,
                         2016-birthyr >= 28 | 2016-birthyr < 38 ~ 3,
                         2016-birthyr >= 38 | 2016-birthyr < 48 ~ 4,
                         2016-birthyr >= 48 | 2016-birthyr < 58 ~ 5,
                         2016-birthyr >= 58 | 2016-birthyr < 68 ~ 6,
                         2016-birthyr >= 68 | 2016-birthyr < 78 ~ 7,
                         2016-birthyr >= 78 | 2016-birthyr < 88 ~ 8,
                         2016-birthyr >= 88 ~ 9),
         employment = case_when(employ %in% c(1,2) ~ 1,
                                employ %in% c(3,4,5,6,7,8,NA) ~ 2),
         marital = case_when(marstat == 1 ~ 1,
                             marstat != 1 ~ 2),
         citizen = case_when(immstat != 2 ~ 1,
                             immstat == 2 ~ 2)
         )
states <- data.frame(inputstate = seq(1:56), 
                     state = tolower(c("AL","AK","","AZ","AR","CA","","CO","CT","DE","DC",
                                       "FL","GA","","HI","ID","IL","IN","IA","KS","KY","LA",
                                       "ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH",
                                       "NJ","NM","NY","NC","ND","OH","OK","OR","PA","","RI","SC",
                                       "SD","TN","TX","UT","VT","VA","","WA","WV","WI","WY")))
cces16 <- left_join(cces16, states, by = "inputstate")

cces16_slim <- cces16 %>% select(state, education, race, CC16_364c, CC16_301a)
  
# process ACS data
# rescale weights by diving by the max weight for any individual in any state
acs_cell_counts$rescaled_n <- acs_cell_counts$n/max(state_weights$max_weight)
acs_cell_counts_slim <- acs_cell_counts %>% select(state, education, race, rescaled_n) #***

# create svydesign object
# assume SRS for illustrative purposes
cces16.des <- svydesign(ids = ~ 1, data = cces16_slim)
cces16.des.ps <- postStratify(design = cces16.des,
                              strata = ~state+race+education,
                              population = acs_cell_counts_slim,
                              partial = TRUE)

# weighted to true ACS
voteshare.weighted <- as.data.frame(svytable(~CC16_364c+race, design=cces16.des.ps)) %>% 
  rename(preference=CC16_364c, share=Freq) %>% 
  mutate(preference = case_when(preference == 1 ~ "Trump",
                                preference == 2 ~ "Clinton",
                                preference == 3 ~ "Johnson",
                                preference == 4 ~ "Stein",
                                preference == 5 ~ "Other",
                                preference == 6 ~ "Won't vote",
                                preference == 7 ~ "Not sure",
                                preference %in% c(8,9, NA) ~ "Skipped/not asked"),
         race = case_when(race == 1 ~ "White",
                          race == 2 ~ "Black",
                          race == 3 ~ "Hispanic",
                          race == 4 ~ "Asian",
                          race == 5 ~ "Other")) %>% 
  group_by(race) %>% mutate(share = share/sum(share))
voteshare.weighted %>% View()

# unweighted
cces16.unweighted.des <- svydesign(ids = ~ 1, data = cces16_slim)
unweighted.res <- as.data.frame(svytable(~CC16_364c+race, design=cces16.unweighted.des)) %>% 
  rename(preference=CC16_364c, share=Freq) %>% 
  mutate(preference = case_when(preference == 1 ~ "Trump",
                                preference == 2 ~ "Clinton",
                                preference == 3 ~ "Johnson",
                                preference == 4 ~ "Stein",
                                preference == 5 ~ "Other",
                                preference == 6 ~ "Won't vote",
                                preference == 7 ~ "Not sure",
                                preference %in% c(8,9, NA) ~ "Skipped/not asked"),
         race = case_when(race == 1 ~ "White",
                          race == 2 ~ "Black",
                          race == 3 ~ "Hispanic",
                          race == 4 ~ "Asian",
                          race == 5 ~ "Other")) %>% 
  group_by(race) %>% mutate(share = share/sum(share))
unweighted.res %>% View()

# weighted to noisy ACS
epsilon <- 0.5
scale <- max(state_weights$max_weight)/epsilon
noisy_acs <- acs_cell_counts_slim
noisy_acs$noisy_n <- noisy_acs$rescaled_n + rlap(mu=0, b=scale, size=nrow(noisy_acs))
noisy_acs$noisy_n <- ifelse(noisy_acs$noisy_n < 0, 1, noisy_acs$noisy_n)
noisy_acs_slim <- noisy_acs %>% select(all_vars, noisy_n)

cces16.noisy.des <- svydesign(ids = ~ 1, data = cces16_slim)
cces16.noisy.des.ps <- postStratify(design = cces16.des,
                              strata = ~all_vars,
                              population = noisy_acs_slim,
                              partial = TRUE)

voteshare.noisy.weighted <- as.data.frame(svytable(~CC16_364c+race, design=cces16.noisy.des.ps)) %>% 
  rename(preference=CC16_364c, share=Freq) %>% 
  mutate(preference = case_when(preference == 1 ~ "Trump",
                                preference == 2 ~ "Clinton",
                                preference == 3 ~ "Johnson",
                                preference == 4 ~ "Stein",
                                preference == 5 ~ "Other",
                                preference == 6 ~ "Won't vote",
                                preference == 7 ~ "Not sure",
                                preference %in% c(8,9, NA) ~ "Skipped/not asked"),
         race = case_when(race == 1 ~ "White",
                          race == 2 ~ "Black",
                          race == 3 ~ "Hispanic",
                          race == 4 ~ "Asian",
                          race == 5 ~ "Other")) %>% 
  group_by(race) %>% mutate(share = share/sum(share))
voteshare.noisy.weighted %>% View()


# combine to compare private versus non-private release
combined <- merge(voteshare.weighted, voteshare.noisy.weighted, 
                  by=c("preference", "race"), suffixes=c("_true", "_noisy"))


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
