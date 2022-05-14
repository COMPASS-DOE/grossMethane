library(tibble)

# Test data
x <- tibble(time = c(0.0, 0.45, 0.92, 1.4, 1.8, 2.3, 2.8),
            cal12CH4ml = c(734, 731, 729, 729, 728, 725, 725) / 1000000,
            cal13CH4ml = c(103, 103, 102, 102, 102, 101, 101) / 1000000,
            # Calculate atom percent (AP) of 13C methane in sample over time
            AP_obs = cal13CH4ml / (cal12CH4ml + cal13CH4ml) * 100)

# Constants
FRAC_K <- 0.98 # 13C consumption as a fraction of 12C consumption (alpha)
FRAC_P <- 0.01 # 13C production as a fraction of 12C production
AP_P <- FRAC_P / (1 + FRAC_P) * 100
VOL_ML <- 100   # Note that currently this isn't used anywhere below

# Prediction function
# t: vector of time values, days
# m0: amount of total methane at time zero
# n0: amount of labeled methane at time zero
# p: production rate of total methane, ml/day
# k: first-order rate constant for methane consumption, 1/day
# Returns AP (atom percent) predictions for each element of t
prediction <- function(time, m0, n0, P, k) {
    # Combined, this is Eq. 11 from von Fischer and Hedin 2002, 10.1029/2001GB001448
    # ...except modified for what I think are two mistakes
    # 1. Added *100 so that the left part is correctly a percent (per their Appendix A)
    # 2. We've just predicted nt and mt, so now doesn't APt flow directly from them?!?
    # How does it make sense to add AP_P (as in vF&H eq. 10 and 11)?

    # Equation 9 (and numerator in Eq. 11):
    nt <- n0 * exp(-k * FRAC_K * time)
    # Equation 5 (and denominator in Eq. 11):
    mt <- (P/k - (P/k - m0) * exp(-k * time))
    # Modified Equation 10/11
    nt / mt * 100 # + AP_P
}

# Cost function - takes params p and k, set by optimizer, and also x
cost_function <- function(params, x) {
#    message(params["P"], ",", k = params["k"])
    AP_pred <- prediction(time = x$time,
                          m0 = x$cal12CH4ml[1] + x$cal13CH4ml[1],
                          n0 = x$cal13CH4ml[1],
                          P = params["P"],
                          k = params["k"])

    # compute SS and return to the optimizer
    sum((AP_pred - x$AP_obs) ^ 2)
}


# main

# General purpose optimizer

# Estimate starting k by slope of 13C
# This follows paragraph 21 in section 2.4
m <- lm(log(cal13CH4ml) ~ time, data = x)
k0 <- unname(m$coefficients["time"]) * 1 / FRAC_K

# Let optim() try different values for P and k until it finds best fit to data
result <- optim(par = c("P" = 0.1, "k"= k0),
                gr = NULL,
                fn = cost_function,
#                method = "L-BFGS-B",
#                lower = c("P" = 0.0, "k"= 0.0),
#                upper = c("P" = Inf, "k"= Inf),
                x)

message("Optimizer solution:")
print(result)

# Predict based on the optimized parameters
message("Predictions:")
x$AP_pred <- prediction(time = x$time,
                     m0 = x$cal12CH4ml[1] + x$cal13CH4ml[1],
                     n0 = x$cal13CH4ml[1],
                     P = result$par["P"],
                     k = result$par["k"])
print(x)

# Plot!
library(ggplot2)
theme_set(theme_bw())

comparison <- ggplot(x, aes(time, AP_obs)) + geom_point() +
    geom_line(aes(y = AP_pred), linetype = 2) +
    ggtitle(paste0("P = ", round(result$par["P"], 8), ", k = ", round(result$par["k"], 3)))
print(comparison)
