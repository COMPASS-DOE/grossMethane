# This code is for analyzing
# gross methane fluxes from GCREW soils.
# 13C tracer incubations were conducted the last week of July 22
# in wetland edge (aka lowland aka transition) and upland soils

# September 2022
# Kendalynn A. Morris

library(dplyr)
library(ggplot2)
library(ggpmisc)
library(car)

Olabs <- c("lowland", "midslope", "upslope", "midstream", "upstream")
Llabs <- c("lowland", "midslope", "upslope")

#read incubation data
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
    # filter out standards and clean up columns
    filter(! id %in% c("SERC100ppm", "UMDzero", "SERCzero", "desert5000")) %>%
    mutate(Timestamp = mdy_hm(`Date/Time`, tz = "UTC"),
           `HR 12CH4 Mean` = if_else(round != "T0", `HR 12CH4 Mean` * 1.083, `HR 12CH4 Mean`),
           `HR 13CH4 Mean` = if_else(round != "T0", `HR 13CH4 Mean` * 1.083, `HR 13CH4 Mean`)) %>%
    select(Timestamp, id, round, vol,
           `HR 12CH4 Mean`, `HR 12CH4 Std`,
           `HR 13CH4 Mean`, `HR 13CH4 Std`,
           `HR Delta iCH4 Mean`, `HR Delta iCH4 Std`,
           `12CO2 Mean`, `12CO2 Std`,
           `13CO2 Mean`, `13CO2 Std`,
           `Delta 13CO2 Mean`, `Delta 13CO2 Std`,
           notes) %>%
    # calculate elapsed time for each sample
    arrange(id, round) %>%
    group_by(id) %>%
    mutate(time_days = difftime(Timestamp, min(Timestamp),
                                units = "days"),
           time_days = as.numeric(time_days)) -> deltas

#write.csv(deltas, "incDeltas.csv")

#read in soil moisture data
soil <- read.csv("field-measurements/July22_soilmoisture.csv")
g_dat <- merge(soil, deltas, by = "id")
#calculate soil dry mass and fresh water mass
g_dat$mass <- g_dat$jdry - g_dat$jempty
g_dat$sm <- (g_dat$jfresh - g_dat$jdry)/g_dat$mass

incdat_out %>%
    mutate(Collar = as.character(id)) %>%
    select(-vol, -id) -> incdat_join

g_dat %>%
    mutate(Mppm = `HR 12CH4 Mean` * 2,
           Mstd = `HR 12CH4 Std` * 2,
           M13ppm = `HR 13CH4 Mean` * 2,
           M13std = `HR 13CH4 Std` * 2,
           Cppm = `12CO2 Mean` * 2,
           Cstd = `12CO2 Std` * 2,
           C13ppm = `13CO2 Mean` * 2,
           C13std = `13CO2 Mean` * 2,
           Collar = as.character(id)) %>%
    select(-id, -jempty, -jfresh, -jdry, -vol,
           -`HR 12CH4 Mean`, -`HR 12CH4 Std`,
           -`HR 13CH4 Mean`, -`HR 13CH4 Std`,
           -`12CO2 Mean`, -`12CO2 Std`,
           -`13CO2 Mean`, -`13CO2 Std`,
           -notes, -Timestamp, -time_days) %>%
        left_join(incdat_join, by = c("Collar", "round")) %>%
    select(order(colnames(.))) %>%
    select(Collar, round, order, Location, Origin,
           mass, sm, everything()) -> data

#calculate umol of gas per g dry soil per day for Production
data$umolPg <- (data$Pt * 0.0446)/g_dat$mass
#calculate umol of gas per g dry soil per day for Consumption
data$umolCg <- (data$Ct * 0.0446)/data$mass

#are data normally distributed?
par(mfrow = c(2, 3))
for (x in 10:14) {
    var = names(g_dat)[x]
    i <- g_dat[,x]
    hist(i,
         main = paste(var))
}
#fluxes should be transformed
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
  rename(Collar = id) %>%
  relocate(c(jempty, jfresh, jdry), .after = rtK) %>%
  left_join(f_dat4g_dat, by = "Collar") -> data

#write.csv(data, "PairedData.csv")

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

