###########
# Utility functions needed to perform DP mechanism and evaluate its impact
###########

sgn <- function(x) {
  return(ifelse(x < 0, -1, 1))
}

rlap <- function(mu=0, b=1, size=1) {
  p <- runif(size) - 0.5
  draws <- mu - b * sgn(p) * log(1 - 2 * abs(p))
  return(draws)
}

rmse <- function(true, pred) {
  return(sqrt(mean((pred-true)^2)))
}