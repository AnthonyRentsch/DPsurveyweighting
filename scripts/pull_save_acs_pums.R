###########
# Download ACS data and save each state to separate file
# Expected runtime: 34 minutes
###########

# Set up
rm(list = ls())
require(plyr); require(dplyr)
setwd("~/Desktop/Harvard/S19/cs208/DPsurveyweighting/")
# setwd("~/Desktop/Bhaven/Harvard/Classes/CS208/DPsurveyweighting/");
# cell_counts_file_name = "cell_counts_5var_newAge.csv"
# state_weights_file_name = "state_weights.csv"

states <- tolower(c(state.abb, "DC"))
person_vars <- c("ST", #state code
                 "AGEP", #age
                 #"CIT", #citizenship
                 #"MAR", #marital status
                 "SCHL", #educational attainment
                 "SEX", #sex/gender
                 #"ESR", #employment status
                 "RAC1P", #race/ethnicity
                 "HISP", #need Hispanic category
                 #"PAOC",
                 #"PINCP", #person's income
                 "PWGTP" #person's weight
                 )

get_acs_data <- function(state, base_url="https://www2.census.gov/programs-surveys/acs/data/pums/2012/5-Year/csv_p", base_file="ss12p"){
  url <- paste0(base_url, state, ".zip")
  file <- paste0(base_file, state, ".csv")
  temp <- tempfile()
  download.file(url, temp)
  data <- read.csv(unz(temp, file))
  unlink(temp)
  return(data)
}


start_time <- Sys.time();
cat(start_time);

for(state in states) { #go through different states
  cat("State:", state)
  # grab ACS data for a state
  state_person_data <- get_acs_data(state);
  # pre-process
  state_person_data <- state_person_data %>% select(person_vars);
  #write data to CSV file
  file_name <- paste("data/state_pums/", state, "_pums.csv", sep="");
  write.csv(state_person_data, file = file_name, row.names=FALSE);

}

end_time <- Sys.time();
cat("Ran for", end_time-start_time);


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
