###########
# Perform computation of post-stratification weights
###########

# Set up
rm(list = ls())
setwd("~/Desktop/Bhaven/Harvard/Classes/CS208/DPsurveyweighting/")
# install.packages("survey")
require(plyr); require(dplyr); require(ggplot2); require(readr); require(survey)
source("scripts/dp_utils.R")
acs_cell_counts <- read.csv("data/cell_counts_5var.csv")
state_weights <- read.csv("data/state_weights.csv")
acs_cell_counts <- acs_cell_counts[,-1]
cces16 <- read_tsv("data/CCES16_Common_OUTPUT_Jul2017_VV.tab", col_names = TRUE)

# process CCES data
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
         age = case_when(2016-birthyr < 18 | is.na(birthyr) ~ 1,
                         2016-birthyr >= 18 & 2016-birthyr < 35 ~ 2,
                         2016-birthyr >= 35 & 2016-birthyr < 50 ~ 3,
                         2016-birthyr >= 50 & 2016-birthyr < 65 ~ 4,
                         2016-birthyr >= 65 ~ 5)
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
         ban_rifle_stance = case_when(CC16_330d == 1 ~ "Support",
                                        CC16_330d == 2 ~ "Oppose",
                                        CC16_330d %in% c(8,9) | is.na(CC16_330d) ~ "No response")
         ) %>%
  select(state, education, race, sex, preference, age, ban_rifle_stance); #+age,
#factorize the CCES age categories to be from 1-5
# cces16_slim$age <- factor(cces16_slim$age, levels = c(1,2,3,4,5));
  
# process ACS data
# rescale weights by diving by the max weight for any individual in any state
acs_cell_counts$rescaled_n <- acs_cell_counts$n/max(state_weights$max_weight)
acs_cell_counts_slim <- acs_cell_counts %>% select(state, education, race, sex, age, rescaled_n); #+age,
# acs_cell_counts_slim$age <- factor(acs_cell_counts_slim$age, levels = c(1,2,3,4,5));
# acs_cell_counts_slim <- acs_cell_counts_slim[acs_cell_counts_slim$age != 1, ]; #remove rows with age==1

# check if any people in CCES have demographic combinations not in ACS
# will delete these
# in 5 variable case, there is only 1 respondent
cces_combos = cces16 %>% mutate(all_vars = paste("state", state, "race", race, "sex", sex,
                                                 "age", age, "education", education,
                                                 sep="_")) %>% 
  select(all_vars)
acs_combos = acs_cell_counts %>% mutate(all_vars = paste("state", state, "race", race, "sex", sex,
                                                 "age", age, "education", education,
                                                 sep="_")) %>% 
  select(all_vars)

only_in_cces <- which(!cces_combos$all_vars %in% acs_combos$all_vars)
cces16_slim <- cces16_slim[-only_in_cces,]

# create svydesign object
# assume SRS for illustrative purposes
cces16.des <- svydesign(ids = ~ 1, data = cces16_slim)
cces16.des.ps <- postStratify(design = cces16.des,
                              strata = ~state+race+education+sex+age,
                              population = acs_cell_counts_slim,
                              partial = TRUE)

#weight CCES respondents to true ACS counts. Get gun control stance based on race
true.weighted.res <- as.data.frame(svytable(~ban_rifle_stance+race, design=cces16.des.ps)) %>% 
  mutate(race = case_when(race == 1 ~ "White",
                          race == 2 ~ "Black",
                          race == 3 ~ "Hispanic",
                          race == 4 ~ "Asian",
                          race == 5 ~ "Other")) %>% 
  group_by(race) %>% mutate(share = Freq/sum(Freq)) %>% select(race, ban_rifle_stance, share)
# true.weighted.res %>% View()

# unweighted
# cces16.unweighted.des <- svydesign(ids = ~ 1, data = cces16_slim)
# unweighted.res <- as.data.frame(svytable(~preference+race, design=cces16.unweighted.des)) %>% 
#   mutate(race = case_when(race == 1 ~ "White",
#                           race == 2 ~ "Black",
#                           race == 3 ~ "Hispanic",
#                           race == 4 ~ "Asian",
#                           race == 5 ~ "Other")) %>% 
#   group_by(race) %>% mutate(share = Freq/sum(Freq)) %>% select(race, preference, share)
# # unweighted.res %>% View()

# weight to noisy ACS for different values of epsilon and save results of
# various survey questions broken out by a demographic
# CURRENTLY: vote preference in 2016 and race
epsilons <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,2,3,4)
num_iter <- 5
i <- 1

results <- matrix(NA, nrow=length(epsilons)*num_iter*length(unique(cces16_slim$race)), ncol=6)
for (epsilon in epsilons) {
  cat("\nEpsilon:", epsilon)
  for (iter in 1:num_iter) {
    
    # set up noisy ACS counts
    scale <- 2/epsilon
    noisy_acs <- acs_cell_counts_slim
    noisy_acs$noisy_n <- noisy_acs$rescaled_n + rlap(mu=0, b=scale, size=nrow(noisy_acs))
    noisy_acs$noisy_n <- ifelse(noisy_acs$noisy_n < 0, 1, noisy_acs$noisy_n)
    noisy_acs_slim <- noisy_acs %>% select(-rescaled_n)
    
    # compute post-stratification weights with this noisy population data
    cces16.noisy.des <- svydesign(ids = ~ 1, data = cces16_slim)
    cces16.noisy.des.ps <- postStratify(design = cces16.noisy.des,
                                        strata = ~state+race+education+sex+age,
                                        population = noisy_acs_slim,
                                        partial = TRUE)
    
    # compute cross-tab for vote preference by race and isolate Clinton-Trump difference
    noisy.weighted.res <- as.data.frame(svytable(~preference+race, design=cces16.noisy.des.ps)) %>% 
      mutate(race = case_when(race == 1 ~ "White",
                              race == 2 ~ "Black",
                              race == 3 ~ "Hispanic",
                              race == 4 ~ "Asian",
                              race == 5 ~ "Other")) %>% 
      group_by(race) %>% mutate(share = Freq/sum(Freq)) %>% select(race, preference, share)
    
    # compare to non-private benchmarks (true weights and unweighted) and store
    for (race in unique(noisy.weighted.res$race)) {
      clinton.noisy.share <- noisy.weighted.res$share[noisy.weighted.res$race==race & noisy.weighted.res$preference=="Clinton"]
      trump.noisy.share <- noisy.weighted.res$share[noisy.weighted.res$race==race & noisy.weighted.res$preference=="Trump"]
      noisy.dif <- clinton.noisy.share - trump.noisy.share
        
      clinton.true.share <- true.weighted.res$share[true.weighted.res$race==race & true.weighted.res$preference=="Clinton"]
      trump.true.share <- true.weighted.res$share[true.weighted.res$race==race & true.weighted.res$preference=="Trump"]
      true.dif <- clinton.true.share - trump.true.share
      
      clinton.unweighted.share <- unweighted.res$share[unweighted.res$race==race & unweighted.res$preference=="Clinton"]
      trump.unweighted.share <- unweighted.res$share[unweighted.res$race==race & unweighted.res$preference=="Trump"]
      unweighted.dif <- clinton.unweighted.share - trump.unweighted.share
      
      results[i,] <- c(race, epsilon, iter, noisy.dif, true.dif, unweighted.dif)
      i <- i + 1
    }
  }
}

results_df <- as.data.frame(results)
names(results_df) <- c("race", "epsilon", "iteration", "two_party_dif_noisy_weight", 
                       "two_party_dif_true_weight", "two_party_dif_no_weight")
results_df <- results_df %>% 
  mutate(epsilon = as.numeric(as.character(epsilon)),
         two_party_dif_noisy_weight = as.numeric(as.character(two_party_dif_noisy_weight)),
         two_party_dif_true_weight = as.numeric(as.character(two_party_dif_true_weight)),
         two_party_dif_no_weight = as.numeric(as.character(two_party_dif_no_weight)))

avg_results_df <- results_df %>% group_by(epsilon, race) %>% 
  summarise(avg_two_party_dif_noisy_weight = mean(two_party_dif_noisy_weight),
         avg_two_party_dif_true_weight = mean(two_party_dif_true_weight),
         avg_two_party_dif_no_weight = mean(two_party_dif_no_weight)) %>% 
  ungroup() %>% select(race, epsilon, avg_two_party_dif_noisy_weight,
                       avg_two_party_dif_true_weight, avg_two_party_dif_no_weight)

vote_share_difs_race <- ggplot(data=avg_results_df[avg_results_df$epsilon < 2,], aes(x=epsilon)) + 
  geom_line(aes(y=avg_two_party_dif_noisy_weight-avg_two_party_dif_true_weight)) +
  facet_wrap(~race, nrow=1) + 
  labs(x="Epsilon", y="Difference between weighted two-party vote share\ndifference between DP and non-DP ACS release") +
  theme_bw()
# pdf("plots/vote_share_difs_race.pdf", width=10, height=5)
vote_share_difs_race
# dev.off()

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
