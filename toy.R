library(tibble)

# Test data
time <- c(0, .45, .92, 1.4, 1.8, 2.3, 2.8)
cal12CH4ml <- c(734, 731, 729, 729, 728, 725, 725) / 1000000
cal13CH4ml <- c(103, 103, 102, 102, 102, 101, 101) / 1000000

x <- tibble(time = time, cal12CH4ml = cal12CH4ml, cal13CH4ml = cal13CH4ml)
x$ratio <- x$cal13CH4ml/(x$cal12CH4ml+x$cal13CH4ml)

# Constants
FRAC_K <- 0.98 # 13C consumption as a fraction of 12C consumption
FRAC_P <- 0.01 # 13C production as a fraction of 12C production
VOL_ML <- 100   # TODO - THIS DOESN'T ENTER ANYWHERE BELOW

# Prediction function - takes data frame x, and params p and k
# p: ml/day production 12C
# k: fraction consumption, /day
# Returns a data frame with new prediction columns in it
prediction <- function(x, p, k) {

    # This is equation 11 from von Fischer and Hedin 2002
    (x$cal13CH4ml[1] * exp(-k * FRAC_K * x$time)) /
        ((x$cal12CH4ml[1] + x$cal13CH4ml[1]) * exp(-k * x$time)) +
    FRAC_P
}

# Cost function - takes params p and k, set by optimizer, and also x
cost_function <- function(params, x) {
    ratio_pred <- prediction(x, params["p"], params["k"])

    # compute SS and return to the optimizer
    sum((ratio_pred - x$ratio) ^ 2)
}


# main

# General purpose optimizer

# Estimate starting k by slope of 13C

result <- optim(c("p" = 0.1, "k"= -0.01), cost_function, gr = NULL, x)
print(result)

# Predict based on the optimized parameters
x$pred <- prediction(x, result$par["p"], result$par["k"])
print(x)

# Plot!
library(ggplot2)

comparison <- ggplot(x, aes(time, ratio)) + geom_point() +
    geom_line(aes(y = pred), linetype = 2)
print(comparison)
