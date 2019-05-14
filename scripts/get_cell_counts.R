###########
# Generate ACS cell counts
###########

rm(list = ls())
require(plyr); require(dplyr)
setwd("~/Desktop/Harvard/S19/cs208/DPsurveyweighting/data")

file_out <- "acs_cell_counts_clean.csv"

states <- tolower(c(state.abb, "DC"))
person_weight <- "PWGTP"

acs_cell_counts <- matrix(1, nrow=2*5*4*6*51, ncol=6)
i <- 1

start_time <- Sys.time()

for (state in states) {
  cat("\nState:", state)
  state_file_path <- paste("state_pums/", state, "_pums.csv", sep="")
  state_data <- read.csv(state_file_path)
  state_data <- state_data %>% 
    mutate(age = case_when(AGEP >=0 & AGEP < 35 ~ 1,
                           AGEP >= 35 & AGEP < 50 ~ 2,
                           AGEP >= 50 & AGEP < 65 ~ 3,
                           AGEP >= 65 ~ 4),
           education = case_when(SCHL %in% c(seq(1,15,by=1), NA) ~ 1,
                                 SCHL %in% c(16,17) ~ 2,
                                 SCHL %in% c(18,19) ~ 3,
                                 SCHL == 20 ~ 4,
                                 SCHL == 21 ~ 5,
                                 SCHL %in% c(22,23,24) ~ 6),
           race = case_when(RAC1P == 1 & HISP == 1 ~ 1,
                            RAC1P == 2 & HISP == 1 ~ 2,
                            HISP != 1 ~ 3,
                            RAC1P == 6 & HISP == 1 ~ 4,
                            RAC1P %in% c(3,4,5,7,8,9) & HISP == 1 ~ 5)
    ) %>% rename(sex=SEX)
  
  for (sex in c(1,2)) {
    for (race in c(1,2,3,4,5)) {
      for (age in c(1,2,3,4)) {
        for (education in c(1,2,3,4,5,6)) {
          tmp_data <- state_data[state_data$sex==sex &
                             state_data$race==race &
                             state_data$age==age &
                             state_data$education==education,]
          weighted_count <- sum(tmp_data[,person_weight])
          new_row <- c(state,sex,race,age,education,weighted_count)
          acs_cell_counts[i,] <- new_row
          i <- i + 1
        }
      }
    }
  }
}

end_time <- Sys.time()
cat("Ran for:", end_time-start_time)

acs_cell_counts <- as.data.frame(acs_cell_counts)
names(acs_cell_counts) <- c("state","sex","race","age","education","n")

write.csv(acs_cell_counts, file_out)

