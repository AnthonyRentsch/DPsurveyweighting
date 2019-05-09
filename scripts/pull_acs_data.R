###########
# Grab and process ACS data
# Expected runtime: 50 minutes
###########

# Set up
rm(list = ls())
require(plyr); require(dplyr)
setwd("~/Desktop/Harvard/S19/cs208/DPsurveyweighting/data/")
cell_counts_file_name = "cell_counts_5var.csv"
state_weights_file_name = "state_weights.csv"

states <- tolower(c(state.abb, "DC"))
person_vars <- c("ST",
                 "AGEP",
                 #"CIT",
                 #"MAR",
                 "SCHL",
                 "SEX",
                 #"ESR",
                 "RAC1P"
                 #"PAOC",
                 #"PINCP"
                 )
person_weight <- "PWGTP"

get_acs_data <- function(state, base_url="https://www2.census.gov/programs-surveys/acs/data/pums/2012/5-Year/csv_p", base_file="ss12p"){
  url <- paste0(base_url, state, ".zip")
  file <- paste0(base_file, state, ".csv")
  temp <- tempfile()
  download.file(url, temp)
  data <- read.csv(unz(temp, file))
  unlink(temp)
  return(data)
}

get_weighted_cell_counts <- function(data, weight_var, ...) {
  # have to leave group variable arg like this to jive w/ dplyr
  cell_counts <- data %>% 
    group_by_(...) %>% 
    tally_(weight_var) %>% 
    ungroup() %>% 
    mutate(weighted_share_of_state = n/sum(n))
  return(cell_counts)
}

df_list <- list()
state_weights <- matrix(NA, nrow=length(states), ncol=3)
i <- 1
start_time <- Sys.time()

for(state in states) {
  cat("State:", state)
  # grab ACS data for a state
  state_person_data <- get_acs_data(state)
  # pre-process
  state_person_data <- state_person_data %>% 
    mutate(age = case_when(AGEP >=0 & AGEP < 18 ~ 1,
                            AGEP >= 18 & AGEP < 35 ~ 2,
                            AGEP >= 35 & AGEP < 50 ~ 3,
                            AGEP >= 50 & AGEP < 65 ~ 4,
                            AGEP >= 65 ~ 5),
           # citizen = case_when(CIT %in% c(1,2,3,4) ~ 1,
           #                 CIT == 5 ~ 2),
           # marital = case_when(MAR == 1 ~ 1,
           #                 MAR != 1 ~ 2),
           education = case_when(SCHL %in% c(seq(1,15,by=1), NA) ~ 1,
                            SCHL %in% c(16,17) ~ 2,
                            SCHL %in% c(18,19) ~ 3,
                            SCHL == 20 ~ 4,
                            SCHL == 21 ~ 5,
                            SCHL %in% c(22,23,24) ~ 6),
           # employment = case_when(ESR %in% c(1,2,4,5) ~ 1,
           #                 ESR %in% c(NA,3,6) ~ 2),
           race = case_when(RAC1P == 1 & HISP == 1 ~ 1,
                             RAC1P == 2 & HISP == 1 ~ 2,
                             HISP != 1 ~ 3,
                             RAC1P == 6 & HISP == 1 ~ 4,
                             RAC1P %in% c(3,4,5,7,8,9) & HISP == 1 ~ 5)
           ) %>% rename(sex=SEX)
  # get cell counts 
  state_cell_counts <- get_weighted_cell_counts(state_person_data, weight_var=person_weight,
                                                "ST","age","education","race","sex")
                                                #"citizen","marital","employment"
  # append state abbrevation
  state_cell_counts$state <- state
  # add df to list of dfs
  df_list[[i]] <- state_cell_counts
  # save sum of weights and maximum weight
  state_weights[i,] <- c(state, 
                         max(state_person_data[,person_weight]), 
                         sum(state_person_data[,person_weight]))

  i <- i + 1
}

end_time <- Sys.time()
cat("Ran for", end_time-start_time)

all_cell_counts <- do.call(dplyr::bind_rows, df_list)
state_weights_df <- as.data.frame(state_weights)
names(state_weights_df) <- c("state", "max_weight", "sum_weights")

write.csv(all_cell_counts, file=cell_counts_file_name)
write.csv(state_weights_df, file=state_weights_file_name)
        


### person ####
# st - state code (also use state.abb)
# agep - person's age, continuous
# cit - citizenship status
# mar - marital status
# schl - ed attainment
# sex - sex
# esr - employment status recoded (binarize this)
# rac1p - recoded race
# pwgtp - person weight
# paoc - presence and age of own children
# pincp - total person's income

### household ###
# hincp - household income, in household file
# noc - number of own children in household, in household file
# metropolitan - ?
