# This code is for analyzing flux data from GCREW.
# We are using ANOVA to perform statistical analysis
# for data from PairedMethaneLiCorData.R

# Created on July 20, 2022
# Kendal and Mitchell

library(lubridate)
library(dplyr)
library(ggplot2)

#currently reading in both nlme (slow, but used in B&P '02) AND lme4 (faster, confusing syntax)
library(nlme)
library(lme4)

f_dat <- read.csv("field-measurements/licorRTA.csv")
f_dat$date <- as_date(f_dat$date, tz="EST", format = "%m/%d/%Y")
#campaign can then be used as a factor, every collar should have a flux value for each week within this period
#BUT there are missing weeks and missing collars within weeks
f_dat$campaign <- cut(f_dat$date, "week", start.on.monday = FALSE)
f_dat$timestamp <- mdy_hm(f_dat$timestamp, tz="EST")

#are data normally distributed?
par(mfrow = c(2, 3))
for (x in 8:13) {
    var = names(f_dat)[x]
    i <- f_dat[,x]
    hist(i,
         main = paste(var))
}
#SWC zeros should be removed
#fluxes should be log transformed
dev.off()

#CH4
range(f_dat$FCH4)
hist(log10(f_dat$FCH4 + 12))
f_dat$nFCH4 <- log10(f_dat$FCH4 +12)

#CO2
range(f_dat$FCO2)
hist(log10(f_dat$FCO2))
f_dat$nFCO2 <- log10(f_dat$FCO2)

#SWC
#cutting off values below 0.10,
#below this is almost certainly probe malfunction
f_dat$SWC <- replace(f_dat$SWC, f_dat$SWC < 0.10, NA)
hist(f_dat$SWC)

#Here the analytical flow for B&P '02 begins for methane fluxes

#basic linear model
CH4.1 <- gls(nFCH4 ~ campaign,
             data = f_dat,
             na.action=na.omit)
#does not account for repeated measures or treatments

#random intercepts for each sample ID (ie., Collar)
#nlme syntax
CH4.2lme <- lme(nFCH4 ~ campaign,
             random = ~1|Collar,
             data = f_dat,
             na.action = na.omit)

#This should be the code for the same model in lme4 syntax
# CH4.2lmer <- lmer(nFCH4 ~ campaign + (1|Collar),
#              data = f_dat,
#              na.action=na.omit)
#allows intercepts to vary randomly by Collar ID

anova(CH4.1, CH4.2lme) #second model has substantially improved fit
#anova() does not work with products of lme4
#instead summary() has to be used for both and values manually compared

#allows the slopes associated with campaign to vary randomly among Collar IDs
CH4.3 <- update(CH4.2lme, random = ~1+campaign|Collar)

#The code below should do that same thing as the code above,
#but is more explicit

# CH4.3 <- lme(nFCH4 ~ campaign,
#              random = ~1 + campaign|Collar,
#              data = f_dat,
#              na.action = na.omit)

#next step
#anova(CH4.2lme, CH4.3)

