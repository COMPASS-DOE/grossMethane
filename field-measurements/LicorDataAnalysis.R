# This code is for analyzing flux data from GCREW.
# We are using ANOVA to perform statistical analysis
# for data from PairedMethaneLiCorData.R

# Created on July 20, 2022
# Kendal and Mitchell

library(lubridate)
library(dplyr)
library(ggplot2)

f_dat <- read.csv("licorRTA.csv")
f_dat$date <- as.Date(f_dat$date)
f_dat$timestamp <- ymd_hms(f_dat$timestam[], tz="EST")

#are data normally distributed?
par(mfrow = c(2, 3))
for (x in 9:13) {
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
is.na(f_dat$SWC) <- !f_dat$SWC
hist(f_dat$SWC)


#Methane
methane <- aov(nFCH4 ~ Location*Origin + date + SWC + TS,
             data = f_dat)
summary(methane)
par(mfrow=c(2,2))
plot(methane)
dev.off()
TukeyHSD(methane, which = "Location")

#interaction driven by lowland
ggplot(f_dat, aes(week(timestamp), FCH4, fill = Origin)) +
    geom_boxplot(aes(group = interaction(week(timestamp), Origin))) +
    labs(x = "Calendar Week", y = "Flux CH4",
         title = "Interaction of Origin & Location") +
    facet_wrap(Location ~ ., scales = "free") +
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

