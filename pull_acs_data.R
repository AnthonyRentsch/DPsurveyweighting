###########
# Grab and process ACS data
# Expected runtime: 1 hour 25 minutes
###########

# Set up
rm(list = ls())
require(plyr); require(dplyr)

states <- tolower(state.abb)
person_vars <- c("ST","AGEP","CIT","MAR","SCHL","SEX","ESR","RAC1P",
                 "PAOC","PINCP")
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
    mutate(weighted_share = n/sum(n))
  return(cell_counts)
}


# first do this for just person level vars, then try to link household

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

# hincp - household income, in household file
# noc - number of own children in household, in household file
# metropolitan - ?

df_list <- list()
i <- 1
start_time <- Sys.time()
for(state in states) {
  cat("State:", state)
  # grab ACS data for a state
  state_person_data <- get_acs_data(state)
  # pre-process
  state_person_data <- state_person_data %>% 
    mutate(AGEP = case_when(AGEP >=0 | AGEP < 18 ~ 1, 
                            AGEP >= 18 | AGEP < 35 ~ 2,
                            AGEP >= 35 | AGEP < 65 ~ 3,
                            AGEP >= 65 ~ 4),
           CIT = case_when(CIT %in% c(1,2,3,4) ~ 1,
                           CIT == 5 ~ 2),
           MAR = case_when(MAR == 1 ~ 1,
                           MAR != 1 ~ 2),
           SCHL = case_when(SCHL %in% c(seq(1,15,by=1), NA) ~ 1,
                            SCHL %in% c(16,17,18,19) ~ 2,
                            SCHL == 20 ~ 3,
                            SCHL == 21 ~ 4,
                            SCHL %in% c(22,23,24) ~ 5),
           ESR = case_when(ESR %in% c(1,2,4,5) ~ 1,
                           ESR %in% c(NA,3,6) ~ 2),
           RAC1P = case_when(RAC1P == 1 ~ 1,
                             RAC1P == 2 ~ 2,
                             RAC1P == 3 ~ 3,
                             RAC1P == 4 ~ 4,
                             RAC1P == 6 ~ 6,
                             RAC1P == 7 ~ 7,
                             RAC1P %in% c(5,8,9) ~ 5,
                             HISP != 1 ~ 999))
  # get cell counts 
  state_cell_counts <- get_weighted_cell_counts(state_person_data, weight_var=person_weight,
                                                "ST","AGEP","CIT","MAR","SCHL","SEX","ESR","RAC1P")
  # append state abbrevation
  state_cell_counts$state <- state
  # add df to list of dfs
  df_list[[i]] <- state_cell_counts
  i <- i + 1
}

end_time <- Sys.time()
cat("Ran for", end_time-start_time)

all_cell_counts <- do.call(dplyr::bind_rows, df_list)
write.csv(all_cell_counts, file="~/Desktop/Harvard/S19/cs208/DPsurveyweighting/data/cell_counts.csv")
        

