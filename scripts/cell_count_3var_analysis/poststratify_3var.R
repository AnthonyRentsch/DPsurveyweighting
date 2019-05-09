###########
# Perform computation of post-stratification weights
###########

#### Set up
rm(list = ls());
setwd("~/Desktop/Bhaven/Harvard/Classes/CS208/DPsurveyweighting/"); #move working directory to the root of our DPsurveyweighting repo
# install.packages("survey")
require(plyr); require(dplyr); require(readr); require(survey)
source("scripts/dp_utils.R");
acs_cell_counts <- read.csv("data/cell_counts_3var.csv"); # load ACS cell counts
state_weights <- read.csv("data/state_weights.csv"); #load the state weights for normalization
acs_cell_counts <- acs_cell_counts[,-1]; #get rid of index column
cces16 <- read_tsv("data/CCES16_Common_OUTPUT_Jul2017_VV.tab", col_names = TRUE); #load the CCES data

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
#create new columns in CCES data with education and race recoded
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

cces16_slim <- cces16 %>% select(state, education, race, CC16_364c, CC16_301a);
  
# process ACS data
# rescale weights by dividing by the max weight for any individual in any state
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


##### weighted to noisy ACS
num_sims = 5; #number of releases to perform for each epsilon
epsilon <- 0.5
scale <- max(state_weights$max_weight)/epsilon; #calculate scale for adding Laplace noise
noisy_acs <- noisy_acs %>% select("state", "education")
combined <- voteshare.weighted; #dataframe to store results
colnames(combined) <- c("preference", "race", 'share_true');

for(i in 1:num_sims){
  #add Laplace noise to ACS cell counts
  noisy_acs <- acs_cell_counts_slim
  noisy_acs$noisy_n <- noisy_acs$rescaled_n + rlap(mu=0, b=scale, size=nrow(noisy_acs));
  
  #change DP count to 1 if it is less than 0
  noisy_acs$noisy_n <- ifelse(noisy_acs$noisy_n < 0, 1, noisy_acs$noisy_n);
  noisy_acs <- noisy_acs %>% select(state, education, race, noisy_n) #trim noisy_acs df to only the relevant columns
  cces16.noisy.des <- svydesign(ids = ~ 1, data = cces16_slim); #create survey design object using CCES data we want weights for
  cces16.noisy.des.ps <- postStratify(design = cces16.des, #get post-strafication weights
                                      strata = ~state+race+education,
                                      population = noisy_acs,
                                      partial = TRUE);
  #calculate the vote share for each candidate based on race
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
    group_by(race) %>% mutate(share = share/sum(share));
  
  colnames(voteshare.noisy.weighted) <- c("preference", "race", paste("share_noisy",i, sep=""));
  
  combined <- merge(combined, voteshare.noisy.weighted, 
                    by=c("preference", "race"))
  
}


combined_trim <- combined[combined$preference == "Clinton" | combined$preference == "Trump", ];
races <- c("White", "Black", "Hispanic", "Asian", "Other");
rmse_results <- matrix(NA, nrow = length(races), ncol = 2)
for(r in 1:length(races) ){
  
  # r = 1;
  race <- races[r];
  #get clinton data
  clinton <- combined_trim[combined_trim$preference== "Clinton" & combined_trim$race == race, ];
  clinton_true <- clinton[, 3];
  clinton_noisy <- as.numeric(clinton[, 4:(4+num_sims-1)]);
  #get trump data
  trump <- combined_trim[combined_trim$preference== "Trump" & combined_trim$race == race, ];
  trump_true <- trump[, 3];
  trump_noisy <- as.numeric(trump[, 4:(4+num_sims-1)]);
  #calculate the rmse for difference
  true_dif <- clinton_true - trump_true; #calculate true difference
  noisy_difs <- clinton_noisy - trump_noisy;
  mse <- mean( (true_dif - noisy_difs)^2 );
  rmse <- sqrt(mse);
  
  rmse_results[r, ] <- c(race, rmse);
  
  
  
}

rmse_results






# combine to compare private versus non-private release
# combined <- merge(voteshare.weighted, voteshare.noisy.weighted, 
#                   by=c("preference", "race"), suffixes=c("_true", "_noisy"))


