library(tibble)
library(ggplot2)
library(ggpmisc)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
theme_set(theme_bw())


# ----- Read in and clean up data -----

# Get names of data files
files <- list.files("picarro/", pattern = "*.csv", full.names = TRUE)
# Helper function
read_file <- function(f) {
    message("Reading ", f)
    read_csv(f, col_types = "ccdcddcddddddddddddddddddddddccccc") %>%
        mutate(File = basename(f))
}
# Read in and pre-process data
lapply(files, read_file) %>%
    bind_rows() %>%
    # filter out test data (???) and clean up columns
    filter(! id %in% c("SERC100ppm", "UMDzero", "SERCzero", "desert5000")) %>%
    mutate(Timestamp = mdy_hm(`Date/Time`, tz = "UTC")) %>%
    select(Timestamp, id, round, vol,
           `HR 12CH4 Mean`, `HR 13CH4 Mean`, notes) %>%
    # calculate elapsed time for each sample
    arrange(id, round) %>%
    group_by(id) %>%
    mutate(time_days = difftime(Timestamp, min(Timestamp),
                                units = "days"),
           time_days = as.numeric(time_days)) ->
    incdat

# ----- QA/QC -----

# Each of the samples has a vol=2, round=T4 observation with bizarre data
# I assume we want to drop it?
incdat <- filter(incdat, vol > 2)
# The id 10 sample's T5 observation has a bizarre `13CO2 Mean` number
# (767, order of magnitude higher than any other in the column). Assume drop.
#incdat <- filter(incdat, `HR 13CH4 Mean` < 700)

incdat %>%
    mutate(id_numeric = as.numeric(id)) %>%
    pivot_longer(cols = c(`HR 12CH4 Mean`, `HR 13CH4 Mean`)) %>%
    ggplot(aes(round, value, group = id, color = factor(id_numeric))) +
    geom_point() + geom_line() +
    ggtitle("POST DATA EXCLUSION") +
    facet_wrap( ~ name, scales = "free") ->
    p
print(p)
ggsave("./outputs/over_time.png", width = 8, height = 5)

# ----- Unit conversion -----

incdat %>%
    mutate(cal12CH4ml = `HR 12CH4 Mean` * 2.00013, # ppm to ml and correct for dilution
           cal13CH4ml = `HR 13CH4 Mean` * 2.00013, # multiply by 2.00013
           cal12CH4ml = ifelse(round != "T0", cal12CH4ml * 1.07, cal12CH4ml),
           cal13CH4ml = ifelse(round != "T0", cal13CH4ml * 1.07, cal13CH4ml),
           # calculate atom percent (AP) of 13C methane in sample over time
           AP_obs = cal13CH4ml / (cal12CH4ml + cal13CH4ml) * 100,
           C = NA) %>%
    filter(id %in% c("52")) -> incdat
#,"4", "71"

# ----- Constants -----

FRAC_K <- 0.98 # 13C consumption as a fraction of 12C consumption (alpha in Eq. 11)
FRAC_P <- 0.01 # 13C production as a fraction of 12C production
AP_P <- FRAC_P / (1 + FRAC_P) * 100 # 13C atom percent of total methane production
VOL_ML <- 100   # Note that currently this isn't used anywhere below

# ----- Model-fitting functions -----

# Prediction function
# t: vector of time values, days
# m0: amount of total methane at time zero
# n0: amount of labeled methane at time zero
# P: production rate of total methane, ml/day
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


# ----- Main -----

# There are many ways to do this: dplyr's group_by/summarise,
# base R's lapply, etc. Here we use a for loop.

pk_results <- list()

for(i in unique(incdat$id)) {
    message("------------------- ", i)
    dat <- filter(incdat, id == i)

    # Estimate starting k by slope of 13C
    # This follows paragraph 21 in section 2.4
    m <- lm(log((cal13CH4ml+cal12CH4ml) * 1/FRAC_K) ~ time_days, data = dat)
    k0 <- unname(m$coefficients["time_days"])
    message("k0 = ", k0)
    k0 <- min(k0, -0.001) #constraint k0 to values of 0 or less
    # Let optim() try different values for P and k until it finds best fit to data
    result <- optim(par = c("P" = 0.01, "k"= k0),
                    fn = cost_function,
                    # Do we want to constrain the optimizer so it can't produce <0 values for P and k?
                     method = "L-BFGS-B",
                     lower = c("P" = 0.0, "k"= -Inf),
                     upper = c("P" = Inf, "k"= -0.001),

                    # "..." that the optimizer will pass to cost_function:
                    time = dat$time_days,
                    m0 = dat$cal12CH4ml[1] + dat$cal13CH4ml[1],
                    n0 = dat$cal13CH4ml[1],
                    AP_obs = dat$AP_obs)


    message("Optimizer solution:")
    print(result)
    P <- result$par["P"]
    pk_results[[i]] <- tibble(P = P,
                              k = result$par["k"],
                              k0 = k0)

    # Predict based on the optimized parameters
    sample_rows <- incdat$id == i
    incdat[sample_rows, "AP_pred"] <-
        ap_prediction(time = dat$time_days,
                      m0 = dat$cal12CH4ml[1] + dat$cal13CH4ml[1],
                      n0 = dat$cal13CH4ml[1],
                      P = P,
                      k = result$par["k"])
    # Calculate implied consumption based on predictions
    # Ct = (P*time - ([CH4t] - [CH4t-1]))/time
    total_methane <- incdat$cal12CH4ml[sample_rows] + incdat$cal13CH4ml[sample_rows]
    change_methane <- c(0, diff(total_methane))
    change_time <- c(0, diff(incdat$time_days[sample_rows]))
    incdat$C[sample_rows] <- (-change_methane + (P*change_time))/change_time
    #for 52, predicted P (for each time step) is too low
    #to account for change_methane at each time step
    }



pk_results <- bind_rows(pk_results, .id = "id")

# ----- Plot results -----

ap_pred <- ggplot(incdat, aes(time_days)) +
    geom_point(aes(y = AP_obs)) +
    geom_line(aes(y = AP_pred), linetype = 2) +
    facet_wrap(~as.numeric(id), scales = "free") +
    geom_text(data = pk_results, x = 0.6, y = 1.5,
              aes(label = paste("P =", format(P, digits = 1, nsmall = 1)))) +
    geom_text(data = pk_results, x = 0.6, y = 1.4,
              aes(label = paste("k =", format(k, digits = 2, nsmall = 2))))
print(ap_pred)
ggsave("./outputs/ap_pred.png")

print(pk_results)

message("All done.")

#multiply k by overall average mls of methane
# pk_results$ml_k <- pk_results$k * 3
# pk_results$net <- pk_results$P + pk_results$ml_k
#
# pk_results %>%
#     filter(net > 0) -> issues
# unique(issues$id)
# lm_issues <- c("44", "52", "58", "59", "71", "72")
# ln_lm_issues <- c("18", "19", "3", "30", "31", "32", "41", "61", "71", "72", "86")
#
#
# pk_results %>%
#     filter(k > 0) -> same
# unique(same$id)
#
# lm_same <- c("44", "52", "58", "59", "71")
# ln_lm_same <- c("18", "3", "32", "41", "61", "71", "86")
#
# intersect(lm_issues, ln_lm_issues)
# intersect(lm_same, ln_lm_same)
#
# #write.csv(pk_results, "27092022_results.csv")

