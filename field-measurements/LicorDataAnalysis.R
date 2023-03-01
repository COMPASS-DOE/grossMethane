# This code is for analyzing flux data from GCREW.
# We are using ANOVA to perform statistical analysis
# for data from PairedMethaneLiCorData.R

# Created on July 20, 2022
# Kendal and Mitchell

library(lubridate)
library(dplyr)
library(ggplot2)
library(nlme)
library(lme4)

f_dat <- read.csv("field-measurements/licorRTA.csv")
f_dat$date <- as_date(f_dat$date, tz="EST", format = "%m/%d/%Y")
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
#SWC 0 should be removed
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


#Methane
CH4.1 <- gls(nFCH4 ~ campaign,
             data = f_dat,
             na.action=na.omit)
#does not account for repeated measures or treatments

CH4.2lme <- lme(nFCH4 ~ campaign,
             random = ~1|Collar,
             data = f_dat,
             na.action = na.omit)

# CH4.2lmer <- lmer(nFCH4 ~ campaign + (1|Collar),
#              data = f_dat,
#              na.action=na.omit)
#allows intercepts to vary randomly by Collar ID

summary(CH4.1)
summary(CH4.2lme) #second model has substantially improved fit


CH4.3 <- update(CH4.2lme, random = ~1+campaign|Collar)
# CH4.3 <- lme(nFCH4 ~ campaign,
#              random = ~1 + campaign|Collar,
#              data = f_dat[f_dat$Location == "g_low",],
#              na.action = na.omit)
#allows the slopes assocaited with campaign to vary randomly among Collar IDs

anova(CH4.2, CH4.3) #second model has substantially improved fit

#interaction driven by lowland
ggplot(f_dat, aes(week(timestamp), FCH4, fill = Origin)) +
    geom_boxplot(aes(group = interaction(week(timestamp), Origin))) +
    labs(x = "Calendar Week", y = "Flux CH4",
         title = "Interaction of Origin & Location") +
    facet_wrap(Location ~ campaign, scales = "free") +
    theme_bw()


#lowland only
low_meth <- aov(nFCH4 ~ Origin + date + SWC + TS,
               data = f_dat[f_dat$Location == "g_low",])
summary(low_meth)
par(mfrow=c(2,2))
plot(low_meth)
dev.off()
TukeyHSD(low_meth, which = "Origin")

#midland soil in lowland is very high
ggplot(f_dat[f_dat$Location == "g_low",], aes(week(timestamp), FCH4, fill = Origin)) +
    geom_boxplot(aes(group = interaction(week(timestamp), Origin))) +
    labs(x = "Calendar Week", y = "Flux CH4",
         title = "Effect of Origin on Lowland CH4") +
    theme_bw()

#exclude g_mid
alt_low_meth <- aov(nFCH4 ~ Origin + date + SWC + TS,
                data = f_dat[f_dat$Location == "g_low" &
                                 f_dat$Origin != "g_mid",])
summary(alt_low_meth)
par(mfrow=c(2,2))
plot(alt_low_meth)
dev.off()
TukeyHSD(alt_low_meth, which = "Origin")
#without midland soil, SWC content is the most important

#are midland origin soils the wettest?
SWC <- aov(SWC ~ Origin + date,
           data = f_dat[f_dat$Location == "g_low",])
summary(SWC)
TukeyHSD(SWC, which = "Origin")
#yes, soils originating from the midland are wetter than
#lowland, midstream, and upstream soils
#also they are statistically similar to upland soils
#which have the second highest water content

#if midland soils are removed,
#are there still differences?
SWC <- aov(SWC ~ Origin + date,
           data = f_dat[f_dat$Location == "g_low" &
                            f_dat$Origin != "g_mid",])
summary(SWC)
TukeyHSD(SWC, which = "Origin")
#No! Woot Woot!

