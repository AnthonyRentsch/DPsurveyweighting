###########
# Perform computation of post-stratification weights
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
         # employment = case_when(employ %in% c(1,2) ~ 1,
         #                        employ %in% c(3,4,5,6,7,8,9) | is.na(employ) ~ 2),
         # marital = case_when(marstat == 1 ~ 1,
         #                     marstat != 1 ~ 2),
         # citizen = case_when(immstat != 2 | is.na(immstat) ~ 1,
         #                     immstat == 2 ~ 2)
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
# acs_cell_counts$rescaled_n <- acs_cell_counts$n/max(state_weights$max_weight)
acs_cell_counts_slim <- acs_cell_counts %>% select(state, education, race, sex, age, 
                                                   n
                                                   #rescaled_n
                                                   ) 

# check if any people in CCES have demographic combinations not in ACS and delete these
# cces_combos = cces16 %>% mutate(all_vars = paste("state", state, "race", race, "sex", sex,
#                                                  "age", age, "education", education,
#                                                  sep="_")) %>% select(all_vars)
# acs_combos = acs_cell_counts %>% mutate(all_vars = paste("state", state, "race", race, "sex", sex,
#                                                  "age", age, "education", education,
#                                                  sep="_")) %>% select(all_vars)
# 
# only_in_cces <- which(!cces_combos$all_vars %in% acs_combos$all_vars)
# cces16_slim <- cces16_slim[-only_in_cces,]

# Analysis

# assume SRS for illustrative purposes
cces16.des <- svydesign(ids = ~ 1, data = cces16_slim)
cces16.des.ps <- postStratify(design = cces16.des,
                              strata = ~state+race+education+sex+age,
                              population = acs_cell_counts_slim,
                              partial = TRUE)

# we are getting NA weights but are unsure why
# bad_inds <- which(is.na(weights(cces16.des.ps)))
# cces16_slim[bad_inds,] %>% View()

###
# VOTE PREFERENCE BY RACE
###
# our true weights
true.weighted.res.vs <- as.data.frame(svytable(~preference+race, design=cces16.des.ps)) %>% 
  mutate(race = case_when(race == 1 ~ "White",
                          race == 2 ~ "Black",
                          race == 3 ~ "Hispanic",
                          race == 4 ~ "Asian",
                          race == 5 ~ "Other")) %>% 
  group_by(race) %>% mutate(share = Freq/sum(Freq)) %>% select(race, preference, share)

# unweighted
cces16.unweighted.des <- svydesign(ids = ~ 1, data = cces16_slim)
unweighted.res.vs <- as.data.frame(svytable(~preference+race, design=cces16.unweighted.des)) %>% 
  mutate(race = case_when(race == 1 ~ "White",
                          race == 2 ~ "Black",
                          race == 3 ~ "Hispanic",
                          race == 4 ~ "Asian",
                          race == 5 ~ "Other")) %>% 
  group_by(race) %>% mutate(share = Freq/sum(Freq)) %>% select(race, preference, share)

# weight to noisy ACS for different values of epsilon
epsilons <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
num_iter <- 10
i <- 1

results.vs <- matrix(NA, nrow=length(epsilons)*num_iter*length(unique(cces16_slim$race)), ncol=6)
for (epsilon in epsilons) {
  cat("\nEpsilon:", epsilon)
  for (iter in 1:num_iter) {
    
    # set up noisy ACS counts
    scale <- 2*max(state_weights$max_weight)/epsilon
    noisy_acs <- acs_cell_counts_slim
    noisy_acs$noisy_n <- noisy_acs$n + rlap(mu=0, b=scale, size=nrow(noisy_acs))
    noisy_acs$noisy_n <- ifelse(noisy_acs$noisy_n < 0, 1, noisy_acs$noisy_n)
    noisy_acs_slim <- noisy_acs %>% select(-n)
    
    # compute post-stratification weights with this noisy population data
    cces16.noisy.des <- svydesign(ids = ~ 1, data = cces16_slim)
    cces16.noisy.des.ps <- postStratify(design = cces16.noisy.des,
                                        strata = ~state+race+education+sex+age,
                                        population = noisy_acs_slim,
                                        partial = TRUE)
    
    # compute cross-tab for vote preference by race and isolate Clinton-Trump difference
    noisy.weighted.res.vs <- as.data.frame(svytable(~preference+race, design=cces16.noisy.des.ps)) %>% 
      mutate(race = case_when(race == 1 ~ "White",
                              race == 2 ~ "Black",
                              race == 3 ~ "Hispanic",
                              race == 4 ~ "Asian",
                              race == 5 ~ "Other")) %>% 
      group_by(race) %>% mutate(share = Freq/sum(Freq)) %>% select(race, preference, share)
    
    # compare to non-private benchmarks (true weights and unweighted) and store
    for (race in unique(noisy.weighted.res.vs$race)) {
      clinton.noisy.share <- noisy.weighted.res.vs$share[noisy.weighted.res.vs$race==race & noisy.weighted.res.vs$preference=="Clinton"]
      trump.noisy.share <- noisy.weighted.res.vs$share[noisy.weighted.res.vs$race==race & noisy.weighted.res.vs$preference=="Trump"]
      noisy.dif <- clinton.noisy.share - trump.noisy.share
        
      clinton.true.share <- true.weighted.res.vs$share[true.weighted.res.vs$race==race & true.weighted.res.vs$preference=="Clinton"]
      trump.true.share <- true.weighted.res.vs$share[true.weighted.res.vs$race==race & true.weighted.res.vs$preference=="Trump"]
      true.dif <- clinton.true.share - trump.true.share
      
      clinton.unweighted.share <- unweighted.res.vs$share[unweighted.res.vs$race==race & unweighted.res.vs$preference=="Clinton"]
      trump.unweighted.share <- unweighted.res.vs$share[unweighted.res.vs$race==race & unweighted.res.vs$preference=="Trump"]
      unweighted.dif <- clinton.unweighted.share - trump.unweighted.share
      
      results.vs[i,] <- c(race, epsilon, iter, noisy.dif, true.dif, unweighted.dif)
      i <- i + 1
    }
  }
}

results.vs.df <- as.data.frame(results.vs)
names(results.vs.df) <- c("race", "epsilon", "iteration", 
                       "two_party_dif_noisy_weight", "two_party_dif_true_weight", 
                       "two_party_dif_no_weight")
results.vs.df <- results.vs.df %>% 
  mutate(epsilon = as.numeric(as.character(epsilon)),
         two_party_dif_noisy_weight = as.numeric(as.character(two_party_dif_noisy_weight)),
         two_party_dif_true_weight = as.numeric(as.character(two_party_dif_true_weight)),
         two_party_dif_no_weight = as.numeric(as.character(two_party_dif_no_weight)))

avg.results.vs.df <- results.vs.df %>% group_by(epsilon, race) %>% 
  summarise(avg_two_party_dif_noisy_weight = mean(two_party_dif_noisy_weight),
         avg_two_party_dif_true_weight = mean(two_party_dif_true_weight),
         avg_two_party_dif_no_weight = mean(two_party_dif_no_weight)) %>% 
  ungroup() %>% select(race, epsilon, avg_two_party_dif_noisy_weight,
                       avg_two_party_dif_true_weight, avg_two_party_dif_no_weight)

vote_share_difs_race <- ggplot(data=avg.results.vs.df, 
                               aes(x=epsilon)) + 
  geom_line(aes(y=(avg_two_party_dif_noisy_weight-avg_two_party_dif_true_weight)*100)) +
  facet_wrap(~race, nrow=1) + 
  coord_cartesian(ylim=c(-2,2)) + 
  labs(x="Epsilon", 
       #y="Difference between weighted two-party vote share\ndifference between DP and non-DP ACS release\n(% points)"
       y=expression(paste("(Clinton lead)"["noisy"], " - ", "(Clinton lead)"["true"], sep=""))) +
  theme_bw()
pdf("plots/vote_share_difs_race.pdf", width=10, height=5)
vote_share_difs_race
dev.off()

###
# SUPPORT FOR ASSAULT RIFLE BAN BY EDUCATION
###
# our true weights
true.weighted.res.arb <- as.data.frame(svytable(~assault_rifle_ban+education, design=cces16.des.ps)) %>% 
  mutate(education = case_when(education == 1 ~ "No HS",
                          education == 2 ~ "HS graduate",
                          education == 3 ~ "Some college",
                          education == 4 ~ "2-year degree",
                          education == 5 ~ "4-year degree",
                          education == 6 ~ "Post-graduate degree")) %>% 
  group_by(education) %>% mutate(share = Freq/sum(Freq)) %>% select(education, assault_rifle_ban, share)

# unweighted
unweighted.res.arb <- as.data.frame(svytable(~assault_rifle_ban+education, design=cces16.unweighted.des)) %>% 
  mutate(education = case_when(education == 1 ~ "No HS",
                          education == 2 ~ "HS graduate",
                          education == 3 ~ "Some college",
                          education == 4 ~ "2-year degree",
                          education == 5 ~ "4-year degree",
                          education == 6 ~ "Post-graduate degree")) %>% 
  group_by(education) %>% mutate(share = Freq/sum(Freq)) %>% select(education, assault_rifle_ban, share)

# weight to noisy ACS for different values of epsilon
epsilons <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
num_iter <- 10
i <- 1

results.arb <- matrix(NA, nrow=length(epsilons)*num_iter*length(unique(cces16_slim$education)), ncol=6)
for (epsilon in epsilons) {
  cat("\nEpsilon:", epsilon)
  for (iter in 1:num_iter) {
    
    # set up noisy ACS counts
    scale <- 2*max(state_weights$max_weight)/epsilon
    noisy_acs <- acs_cell_counts_slim
    noisy_acs$noisy_n <- noisy_acs$n + rlap(mu=0, b=scale, size=nrow(noisy_acs))
    noisy_acs$noisy_n <- ifelse(noisy_acs$noisy_n < 0, 1, noisy_acs$noisy_n)
    noisy_acs_slim <- noisy_acs %>% select(-n)
    
    # compute post-stratification weights with this noisy population data
    cces16.noisy.des <- svydesign(ids = ~ 1, data = cces16_slim)
    cces16.noisy.des.ps <- postStratify(design = cces16.noisy.des,
                                        strata = ~state+race+education+sex+age,
                                        population = noisy_acs_slim,
                                        partial = TRUE)
    
    # compute cross-tab for vote preference by race and isolate Clinton-Trump difference
    noisy.weighted.res.arb <- as.data.frame(svytable(~assault_rifle_ban+education, design=cces16.noisy.des.ps)) %>% 
      mutate(education = case_when(education == 1 ~ "No HS",
                              education == 2 ~ "HS graduate",
                              education == 3 ~ "Some college",
                              education == 4 ~ "2-year degree",
                              education == 5 ~ "4-year degree",
                              education == 6 ~ "Post-graduate degree")) %>% 
      group_by(education) %>% mutate(share = Freq/sum(Freq)) %>% select(education, assault_rifle_ban, share)
    
    # compare to non-private benchmarks (true weights and unweighted) and store
    for (education in unique(noisy.weighted.res.arb$education)) {
      support.noisy.share <- noisy.weighted.res.arb$share[noisy.weighted.res.arb$education==education & noisy.weighted.res.arb$assault_rifle_ban=="Support"]
      oppose.noisy.share <- noisy.weighted.res.arb$share[noisy.weighted.res.arb$education==education & noisy.weighted.res.arb$assault_rifle_ban=="Oppose"]
      noisy.dif <- support.noisy.share - oppose.noisy.share
      
      support.true.share <- true.weighted.res.arb$share[true.weighted.res.arb$education==education & true.weighted.res.arb$assault_rifle_ban=="Support"]
      oppose.true.share <- true.weighted.res.arb$share[true.weighted.res.arb$education==education & true.weighted.res.arb$assault_rifle_ban=="Oppose"]
      true.dif <- support.true.share - oppose.true.share
      
      support.unweighted.share <- unweighted.res.arb$share[unweighted.res.arb$education==education & unweighted.res.arb$assault_rifle_ban=="Support"]
      oppose.unweighted.share <- unweighted.res.arb$share[unweighted.res.arb$education==education & unweighted.res.arb$assault_rifle_ban=="Oppose"]
      unweighted.dif <- support.unweighted.share - oppose.unweighted.share
      
      results.arb[i,] <- c(education, epsilon, iter, noisy.dif, true.dif, unweighted.dif)
      i <- i + 1
    }
  }
}

results.arb.df <- as.data.frame(results.arb)
names(results.arb.df) <- c("education", "epsilon", "iteration", "two_party_dif_noisy_weight", 
                          "two_party_dif_true_weight", "two_party_dif_no_weight")
results.arb.df <- results.arb.df %>% 
  mutate(epsilon = as.numeric(as.character(epsilon)),
         two_party_dif_noisy_weight = as.numeric(as.character(two_party_dif_noisy_weight)),
         two_party_dif_true_weight = as.numeric(as.character(two_party_dif_true_weight)),
         two_party_dif_no_weight = as.numeric(as.character(two_party_dif_no_weight)))

avg.results.arb.df <- results.arb.df %>% group_by(epsilon, education) %>% 
  summarise(avg_two_party_dif_noisy_weight = mean(two_party_dif_noisy_weight),
            avg_two_party_dif_true_weight = mean(two_party_dif_true_weight),
            avg_two_party_dif_no_weight = mean(two_party_dif_no_weight)) %>% 
  ungroup() %>% select(education, epsilon, avg_two_party_dif_noisy_weight,
                       avg_two_party_dif_true_weight, avg_two_party_dif_no_weight)

avg.results.arb.df$education <- factor(avg.results.arb.df$education, 
                                       levels = c("No HS","HS graduate","Some college",
                                                  "2-year degree","4-year degree",
                                                  "Post-graduate degree"))

assault_rifle_ban_difs_education <- ggplot(data=avg.results.arb.df, aes(x=epsilon)) + 
  geom_line(aes(y=(avg_two_party_dif_noisy_weight-avg_two_party_dif_true_weight)*100)) +
  facet_wrap(~education, nrow=1) + 
  coord_cartesian(ylim=c(-2,2)) + 
  labs(x="Epsilon", y="Difference between weighted net support for assault\nrifle ban between DP and non-DP ACS release\n(% points)") +
  theme_bw()
pdf("plots/assault_rifle_ban_difs_education.pdf", width=10, height=5)
assault_rifle_ban_difs_education
dev.off()

# Appendix
# compare our "true" weights to actual CCES weights

## 
# VOTE SHARE BY RACE 
### 
# cces.16.actual.des <- svydesign(ids = ~1, data = cces16, weights = ~commonweight)
# cces16.actual.vs <- as.data.frame(svytable(~CC16_364c+race, design=cces.16.actual.des)) %>%
#   rename(preference=CC16_364c, share=Freq) %>%
#   mutate(preference = case_when(preference == 1 ~ "Trump",
#                                 preference == 2 ~ "Clinton",
#                                 preference == 3 ~ "Johnson",
#                                 preference == 4 ~ "Stein",
#                                 preference == 5 ~ "Other",
#                                 preference == 6 ~ "Won't vote",
#                                 preference == 7 ~ "Not sure",
#                                 preference %in% c(8,9) ~ "Skipped/not asked"),
#          race = case_when(race == 1 ~ "White",
#                                  race == 2 ~ "Black",
#                                  race == 3 ~ "Hispanic",
#                                  race == 4 ~ "Asian",
#                                  race == 5 ~ "Other")) %>% 
#            group_by(race) %>% mutate(share = share/sum(share)) %>% select(race, preference, share)
# 
# compare.vs <- merge(cces16.actual.vs, true.weighted.res.vs, by=c("race","preference"),
#                         suffixes=c("_actual","_estimate")) %>% 
#   filter(preference %in% c("Clinton","Trump"))
# 
# compare.vs %>% group_by(race) %>% 
#   mutate(net_actual = share_actual - lead(share_actual, default = first(share_actual)),
#          net_estimate = share_estimate - lead(share_estimate, default = first(share_estimate))) %>% 
#   filter(preference=="Clinton") %>% select(race, net_actual, net_estimate) %>% View()
