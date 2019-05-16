###########
# Generate histogram of weights for people in the PUMS files
###########

rm(list = ls())
require(plyr); require(dplyr)
# setwd("~/Desktop/Harvard/S19/cs208/DPsurveyweighting/data")
setwd("~/Desktop/Bhaven/Harvard/Classes/CS208/DPsurveyweighting/data/")

# file_out <- "acs_cell_counts_clean.csv"

states <- tolower(c(state.abb, "DC"))
person_weight <- "PWGTP"

acs_cell_counts <- matrix(1, nrow=2*5*4*6*51, ncol=6)
i <- 1

start_time <- Sys.time()
person_weights <- c(); #vector to hold the person weights

for (state in states) {
  cat("\nState:", state) #print State abbreviation
  state_file_path <- paste("state_pums/", state, "_pums.csv", sep="")
  state_data <- read.csv(state_file_path); #load the state's PUMS data
  
  person_weights <- c(person_weights, as.numeric(state_data$PWGTP));
  
}

end_time <- Sys.time()
cat("Ran for:", end_time-start_time)

#change person weights to dataframe
person_weights_df <- as.data.frame(person_weights)


#plot histogram of weights
require(ggplot2);

p <- ggplot(data = person_weights_df, aes(x=person_weights)) + geom_histogram(bins = 100) +
  scale_x_discrete(name ="Person weights", 
                   limits=c(0,100, 200, 300, 400, 500)) + 
  labs(y="Count") +
  theme_bw() 

pdf("../plots/person_weights_histogram.pdf", width=10, height=5);
p
dev.off()
