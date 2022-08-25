library(tibble)
library(ggplot2)
library(dplyr)
library(lubridate)
theme_set(theme_bw())


# x <- tibble(time = c(0.0, 0.47, 0.60),
#             cal12CH4ml = c(0.0016, 0.0013, 0.0010),
#             cal13CH4ml = c(0.000024, 0.000019, 0.000015),
#             # Calculate atom percent (AP) of 13C methane in sample over time
#             AP_obs = cal13CH4ml / (cal12CH4ml + cal13CH4ml) * 100)

#read in .csv
a <- read.csv("picarro/CFIDS2333_SSIM_20220721_135555.csv")
b <- read.csv("picarro/CFIDS2333_SSIM_20220721_185547.csv")
c <- read.csv("picarro/CFIDS2333_SSIM_20220722_164917.csv")

#no text in this file's id column, so need to tell R to treat it as a character
b$id <- as.character(b$id)

data <- bind_rows(a, b, c)
data$timestamp <- mdy_hm(data$Date.Time, tz = "UTC")

#all data for sample 1, ignore 2 ml sample volume at T4
#select id, round, notes, timestamp, CH4delta, CH4delta std, CO2delta, CO2delta std,
#ppm12CH4, ppm12CH4 std, ppm13CH4, ppm13CH4, ppm12CO2, ppm12CO2 std, ppm13CO2, ppm13CO2,
#ppmH20, ppmH20 std
data %>% filter(id == "1",
             vol == 10) %>%
    select("id","round","vol","notes","timestamp","HR.Delta.iCH4.Mean",
           "HR.Delta.iCH4.Std","Delta.13CO2.Mean","Delta.13CO2.Std",
           "HR.12CH4.Mean","HR.12CH4.Std","HR.13CH4.Mean","HR.13CH4.Std",
           "X12CO2.Mean","X12CO2.Std","X13CO2.Mean","X13CO2.Std","H2O.Mean",
           "H2O.Std") -> x1
#not sure how to calculate time elapsed between timestamps
#also samples were in the fridge overnight between T3 and T4
#using average minutes elapsed (with a fudge factor for the overnight) for now
minutes_elapsed = c(0, 102, 272, 386, 1013, 1176)
#convert to days
time = minutes_elapsed/(60*24)


x <- tibble(time,
cal12CH4ml = x1$HR.12CH4.Mean * 2.00013, #ppm to ml and correct for dilution
cal13CH4ml = x1$HR.13CH4.Mean * 2.00013, #multiply by 2.00013
#Calculate atom percent (AP) of 13C methane in sample over time
AP_obs = cal13CH4ml / (cal12CH4ml + cal13CH4ml) * 100)



#ppm to ml
#multiply by 0.00013 


# Constants
FRAC_K <- 0.98 # 13C consumption as a fraction of 12C consumption (alpha in Eq. 11)
FRAC_P <- 0.01 # 13C production as a fraction of 12C production
AP_P <- FRAC_P / (1 + FRAC_P) * 100 # 13C atom percent of total methane production
VOL_ML <- 100   # Note that currently this isn't used anywhere below

# Prediction function
# t: vector of time values, days
# m0: amount of total methane at time zero
# n0: amount of labeled methane at time zero
# p: production rate of total methane, ml/day
# k: first-order rate constant for methane consumption, 1/day
# Returns AP (atom percent) predictions for each element of t
ap_prediction <- function(time, m0, n0, P, k) {
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

# Cost function called by optim()
# params: named vector holding optimizer-assigned values for P and k
# time: vector of time values, days
# m0: amount of total methane at time zero
# n0: amount of labeled methane at time zero
# AP_obs: observed atom percent for 13C
# Returns the sum of squares between predicted and observed AP
cost_function <- function(params, time, m0, n0, AP_obs) {
    #    message(params["P"], ",", params["k"])
    AP_pred <- ap_prediction(time = time,
                             m0 = m0,
                             n0 = n0,
                             P = params["P"],
                             k = params["k"])

    # Return sum of squares to the optimizer
    sum((AP_pred - AP_obs) ^ 2)
}


# main

# General purpose optimizer

# Estimate starting k by slope of 13C
# This follows paragraph 21 in section 2.4
m <- lm(log(cal13CH4ml) ~ time, data = x)
k0 <- unname(m$coefficients["time"]) * 1 / FRAC_K

# Let optim() try different values for P and k until it finds best fit to data
result <- optim(par = c("P" = 0.1, "k"= k0),
                fn = cost_function,
                # Do we want to constrain the optimizer so it can't produce <0 values for P and k?
                # method = "L-BFGS-B",
                # lower = c("P" = 0.0, "k"= 0.0),
                # upper = c("P" = Inf, "k"= Inf),

                # "..." that the optimizer will pass to cost_function:
                time = x$time,
                m0 = x$cal12CH4ml[1] + x$cal13CH4ml[1],
                n0 = x$cal13CH4ml[1],
                AP_obs = x$AP_obs)

message("Optimizer solution:")
print(result)

# Predict based on the optimized parameters
message("Predictions:")
x$AP_pred <- ap_prediction(time = x$time,
                           m0 = x$cal12CH4ml[1] + x$cal13CH4ml[1],
                           n0 = x$cal13CH4ml[1],
                           P = result$par["P"],
                           k = result$par["k"])
print(x)

# Plot!
comparison <- ggplot(x, aes(time)) + geom_point(aes(y = AP_obs)) +
    geom_line(aes(y = AP_pred), linetype = 2) +
    ggtitle(paste0("P = ", round(result$par["P"], 8), ", k = ", round(result$par["k"], 4)))
print(comparison)

