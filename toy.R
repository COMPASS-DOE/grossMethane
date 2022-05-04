library(tibble)

# Test data
time <- c(0, .45, .92, 1.4, 1.8, 2.3, 2.8)
cal12CH4ml <- c(734, 731, 729, 729, 728, 725, 725) / 1000000
cal13CH4ml <- c(103, 103, 102, 102, 102, 101, 101) / 1000000

x <- tibble(time = time, cal12CH4ml = cal12CH4ml, cal13CH4ml = cal13CH4ml)

# Constants
FRAC_K <- 0.98 # 13C consumption as a fraction of 12C consumption
FRAC_P <- 0.01 # 13C production as a fraction of 12C production
VOL_ML <- 100   # TODO - THIS DOESN'T ENTER ANYWHERE BELOW

# Prediction function - takes data frame x, and params p and k
# p: ml/day production 12C
# k: fraction consumption, /day
# Returns a data frame with new prediction columns in it
prediction <- function(x, p, k) {
    x$deltaT <- c(0, diff(x$time))

    # Production is constant but timesteps vary
    x$prod12CH4 <- p * x$deltaT
    x$prod13CH4 <- x$prod12CH4 * FRAC_P

    # Consumption varies and is predicted below
    x$cons12CH4 <- NA_real_
    x$cons13CH4 <- NA_real_

    # Concentration/volume is also predicted except for the first value
    x$pred12CH4 <- NA_real_
    x$pred12CH4[1] <- x$cal12CH4ml[1]  # first value is observed
    x$pred13CH4 <- NA_real_
    x$pred13CH4[1] <- x$cal13CH4ml[1]  # first value is observed

    # Consumption is a function of previous concentration, k, and time
    # So loop through each timepoint and compute it, and then resulting CH4
    for(i in 2:nrow(x)) {
        x$cons12CH4[i] <- x$pred12CH4[i-1] * k * x$deltaT[i]
        x$pred12CH4[i] <- x$pred12CH4[i-1] + x$prod12CH4[i] - x$cons12CH4[i]

        x$cons13CH4[i] <- x$cons12CH4[i] * FRAC_K
        x$pred13CH4[i] <- x$pred13CH4[i-1] + x$prod13CH4[i] - x$cons13CH4[i]
    }

    x
}

# Cost function - takes params p and k, set by optimizer, and also x
cost_function <- function(params, x) {
    x_pred <- prediction(x, params["p"], params["k"])

    # compute normalized SS and return to the optimizer
    with(x_pred, sum((pred12CH4 - cal12CH4ml) ^ 2 / cal12CH4ml +
                         (pred13CH4 - cal13CH4ml) ^ 2 / cal12CH4ml))
}


# main

# General purpose optimizer
result <- optim(c("p" = 0.1, "k"= 0.1), cost_function, gr = NULL, x)
print(result)

# Predict based on the optimized parameters
x_pred <- prediction(x, result$par["p"], result$par["k"])
print(x_pred)

# Plot!
library(ggplot2)

p12C <- ggplot(x_pred, aes(time, cal12CH4ml)) + geom_point() +
    geom_line(aes(y = pred12CH4), linetype = 2)
print(p12C)

p13C <- ggplot(x_pred, aes(time, cal13CH4ml)) + geom_point() +
    geom_line(aes(y = pred13CH4), linetype = 2)
print(p13C)
