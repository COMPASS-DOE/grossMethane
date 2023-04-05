# This code is for analyzing flux data from GCREW.
# We are using ANOVA to perform statistical analysis
# for data from PairedMethaneLiCorData.R

# Created on July 20, 2022
# Kendal and Mitchell

library(lubridate)
library(dplyr)
library(sjPlot)
library(nlme)


f_dat <- read.csv("field-measurements/licorRTA.csv")
f_dat$date <- as_date(f_dat$date, tz="EST", format = "%m/%d/%Y")
#campaign can then be used as a factor, every collar should have a flux value for each week within this period
#BUT there are missing weeks and missing collars within weeks
f_dat$campaign <- cut(f_dat$date, "week", start.on.monday = FALSE)
f_dat$campaign <- ymd(f_dat$campaign, tz="EST")
f_dat$camp_numeric <- as.numeric(as.factor(f_dat$campaign))
f_dat$timestamp <- mdy_hm(f_dat$timestamp, tz="EST")
f_dat$type <- as.factor(paste(as.character(f_dat$Location), as.character(f_dat$Origin)))

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
CH4.1 <- gls(nFCH4 ~ date,
             data = f_dat,
             na.action=na.omit)
#does not account for repeated measures or treatments

#random intercepts for each sample ID (ie., Collar)
#nlme syntax
CH4.2 <- lme(nFCH4 ~ date,
             random = ~1|Collar,
             data = f_dat,
             na.action = na.omit)


#compare the two
anova(CH4.1, CH4.2)
#second model has substantially improved fit


#allows the slopes associated with date to vary randomly among Collar IDs
CH4.3 <- update(CH4.2, random = ~ 1 + date|Collar)

#these two models are not meaningfully different from each other for this data
anova(CH4.2, CH4.3)
#there is also low auto-correlation in the data,
#which likely explains by allowing the slope of date
#to differ for each collar does not improve dit
VarCorr(CH4.3)


#so far "date" has been used for tracking time, but it would be better to use campaign
#B/C there are fewer missing values by grouping weekly
CH4.2_campaign <- lme(nFCH4 ~ camp_numeric,
             random = ~1|Collar,
             data = f_dat,
             na.action = na.omit)
anova(CH4.2, CH4.2_campaign)
#there is a marginal improvement with campaign vs date
#keeping the model with campaign as it more accurately
#represents how data were collected


#to test heteroscedasticity of errors, the variance exponent is weighted by campaign
CH4.4 <- update(CH4.2_campaign, weights = varExp(form=~camp_numeric))
anova(CH4.4, CH4.2_campaign)
#this substantial improves fit
#and makes sense, fluxes should start small at the beginning of the growing season
#and increase in variation as the growing season progresses

#now to bring in additional fixed effects
#options: CO2 flux, SWC, Location, and/or Origin (both factors combined coded as 'type')
CH4.5aprime <- lme(nFCH4 ~ camp_numeric + nFCO2 + SWC + Location + Origin,
              random = ~ camp_numeric|Collar,
              data = f_dat,
              weights = varExp(form = ~camp_numeric),
              na.action = na.omit,
              method = "ML")

CH4.5a <- lme(nFCH4 ~ camp_numeric + nFCO2 + SWC + type,
             random = ~ camp_numeric|Collar,
             data = f_dat,
             weights = varExp(form = ~camp_numeric),
             na.action = na.omit,
             method = "ML")

CH4.5b <- lme(nFCH4 ~ camp_numeric + nFCO2 + SWC + Location,
              random = ~ 1|Collar,
              data = f_dat,
              weights = varExp(form = ~camp_numeric),
              na.action = na.omit,
              method = "ML")

CH4.5c <- lme(nFCH4 ~ camp_numeric + nFCO2 + SWC,
              random = ~ 1|Collar,
              data = f_dat,
              weights = varExp(form = ~camp_numeric),
              na.action = na.omit,
              method = "ML")

CH4.5d <- lme(nFCH4 ~ camp_numeric + SWC,
              random = ~ 1|Collar,
              data = f_dat,
              weights = varExp(form = ~camp_numeric),
              na.action = na.omit,
              method = "ML")

AIC(CH4.5aprime, CH4.5a, CH4.5b, CH4.5c, CH4.5d)
BIC(CH4.5aprime, CH4.5a, CH4.5b, CH4.5c, CH4.5d)

#most complex model wins out
summary(CH4.5a)


plot_model(CH4.5a)
tab_model(CH4.5a)
#additional visual ideas
#https://lmudge13.github.io/sample_code/mixed_effects.html
