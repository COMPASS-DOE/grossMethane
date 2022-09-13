# This code is for analyzing and graphing
# gross methane fluxes from GCREW soils.
# 13C tracer incubations were conducted the last week of July 22
# in wetland edge (aka lowland aka transition) and upland soils

# September 2022
# Kendalynn A. Morris

#read gross rate results generated on September 13th
#from commit "venture into toyland"

library(dplyr)
library(ggplot2)
library(ggpmisc)
library(car)

Olabs <- c("lowland", "midslope", "upslope", "midstream", "upstream")
Llabs <- c("lowland", "midslope", "upslope")

pk_results <- read.csv("13092022_pk_results.csv")

#read in soil moisture data
soil <- read.csv("field-measurements/July22_soilmoisture.csv")
g_dat <- merge(soil, pk_results, by = "id")
#calculate soil dry mass and fresh water mass
g_dat$mass <- g_dat$jdry - g_dat$jempty
g_dat$sm <- (g_dat$jfresh - g_dat$jdry)/g_dat$mass
#calculate umol of gas per g dry soil per day for Production
g_dat$umolPg <- (g_dat$P * 44.64)/g_dat$mass
#calculate umol of gas per g dry soil per day for Consumption
g_dat$umolKg <- (g_dat$k * 44.64)/g_dat$mass

#are data normally distributed?
par(mfrow = c(2, 3))
for (x in 10:14) {
    var = names(g_dat)[x]
    i <- g_dat[,x]
    hist(i,
         main = paste(var))
}
#fluxes should be log transformed
dev.off()

hist(log10(g_dat$umolPg + 50))
g_dat$logP <- log10(g_dat$umolPg + 50)
hist(sqrt(g_dat$umolKg + 83))
g_dat$rtK <- sqrt(g_dat$umolPg + 83)

#read in field flux data for July 27th and 29th
f_dat <- read.csv("field-measurements/licorRTA.csv")

f_dat %>%
  filter(date %in% c("2022-07-27", "2022-07-29")) %>%
  select(-X) -> f_dat4g_dat

g_dat %>%
  select(-X, -P5, -k5) %>%
  rename(Collar = id) %>%
  relocate(c(jempty, jfresh, jdry), .after = rtK) %>%
  left_join(f_dat4g_dat, by = "Collar") -> data

consumption <- aov(rtK ~ Location + sm,
               data = data)
Anova(consumption, type="III")
par(mfrow=c(2,2))
plot(consumption)
dev.off()

production <- aov(logP ~ Location + sm,
                   data = data)
Anova(production, type="III")
par(mfrow=c(2,2))
plot(production)
dev.off()

