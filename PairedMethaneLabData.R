# This code is for analyzing
# gross methane fluxes from GCREW soils.
# 13C tracer incubations were conducted the last week of July 22
# soils from wetland edge (aka lowland aka transition) and upland collars

# September 2022
# Kendalynn A. Morris

# will not run without products of CH4_PoolDilution

library(car)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(lubridate)
library(rcompanion)
library(readr)

Olabs <- c("lowland", "midslope", "upland", "midstream", "upstream")
Llabs <- c("lowland", "midslope", "upland")

#Read Picarro data

# Get names of data files
files <- list.files("picarro/", pattern = "*.csv", full.names = TRUE)
# Helper function
read_file <- function(f) {
    message("Reading ", f)
    read_csv(f, col_types = "ccdcddcddddddddddddddddddddddccccc") %>%
        mutate(File = basename(f))
}
# select desired columns, correct for dilution with zero air
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
           #ignoring dilution of CO2 with zero air for now
           `12CO2 Mean`, `12CO2 Std`,
           `13CO2 Mean`, `13CO2 Std`,
           `Delta 13CO2 Mean`, `Delta 13CO2 Std`) -> picDat

#read in soil moisture data
smDat <- read_csv("field-measurements/July22_soilmoisture.csv",
                  col_types = "ccdddcc")

#combine lab data (no flux data yet)
labDat <- merge(smDat, picDat, by = "id")
#calculate soil dry mass and fresh water mass
labDat$mass <- labDat$jdry - labDat$jempty
labDat$sm <- (labDat$jfresh - labDat$jdry)/labDat$mass

#select columns from CH4_PoolDilution output
#name of data frame might change!
incdat_out %>%
    select(-c(vol, convergence, message)) -> incdat_join

#rename columns,
#correct for 50% dilution of injected sample volume
#should std's be adjusted??? probably
#check equations in analytical chemistry textbook
labDat %>%
    mutate(Mppm = `HR 12CH4 Mean` * 2,
           Mstd = `HR 12CH4 Std`,
           M13ppm = `HR 13CH4 Mean` * 2,
           M13std = `HR 13CH4 Std`,
           Cppm = `12CO2 Mean` * 2,
           Cstd = `12CO2 Std`,
           C13ppm = `13CO2 Mean` * 2,
           C13std = `13CO2 Std`,
           delC = `Delta 13CO2 Mean`,
           delCstd = `Delta 13CO2 Std`,
           delM = `HR Delta iCH4 Mean`,
           delMstd = `HR Delta iCH4 Std`) %>%
    select(-jempty, -jfresh, -jdry, -vol,
           -`HR 12CH4 Mean`, -`HR 12CH4 Std`,
           -`HR 13CH4 Mean`, -`HR 13CH4 Std`,
           -`12CO2 Mean`, -`12CO2 Std`,
           -`13CO2 Mean`, -`13CO2 Std`,
           -`Delta 13CO2 Mean`, -`Delta 13CO2 Std`,
           -`HR Delta iCH4 Mean`, -`HR Delta iCH4 Std`,
           -order) %>%
        left_join(incdat_join, by = c("id", "round")) %>%
    select(order(colnames(.))) %>%
    arrange(id, round) %>%
    select(id, round, Location, Origin,
           mass, sm, Pt, Ct, everything()) %>%
    group_by(id) %>% mutate(change_time = c(0, diff(time_days))) %>%
    ungroup() -> data

#ml to umols, n = (RT/PV)*10^6
#R = 82.0574 mL * atm/K*mol
#T = 297.15 K, estimated lab temp
#P = 1 atm
#V = Pt or Ct

data$umolP <- (data$Pt/(82.0574*297.15)) * 10^6
data$umolC <- (data$Ct/(82.0574*297.15)) * 10^6

#convert to rate per g dry soil per day
data$P_rate <- (data$umolP/data$time_days)/data$mass
data$C_rate <- (data$umolC/data$time_days)/data$mass

#read in field flux data for July 27th and 29th
#the two days when soil was collected for incubations
fDat <- read_csv("field-measurements/licorRTA.csv",
                 col_types = "cdcccccddddd")
fDat %>%
    filter(date %in% c("7/27/2022", "7/29/2022")) %>%
    mutate(id = Collar,
           f_time = timestamp) %>%
    select(-Collar, -date, -Origin, -Location, -timestamp) -> incDays

#data Ready-To-Analyze
data %>%
    mutate(l_time = Timestamp) %>%
    select(-Timestamp) %>%
    left_join(incDays, by = "id") -> dataRTA

#write.csv(dataRTA, "PairedData.csv")

#preliminary analysis

#are data normally distributed?
par(mfrow = c(4,4))
for (x in 6:32) {
    var = names(dataRTA)[x]
    i <- data[,x]
    hist(i,
         main = paste(var))
}
par(mfrow = c(1,1))
#fluxes should be transformed

#fragile code here!

#first round, so not influence by time
#nor duplicate values
CPdat <- data[data$round == "T1",]

###
#production
###
hist(CPdat$umolPg)
qqnorm(CPdat$umolPg);qqline(CPdat$umolPg)
transformTukey(CPdat$umolPg,
               start = -10, end = 10, int = 0.01,
               plotit = TRUE, verbose = FALSE, statistic = 1)
#lamba = 0.26
hist((CPdat$umolPg)^0.26)
qqnorm(CPdat$umolPg^0.26);qqline(data$CPdat^0.26)

#P analysis
###Looked at Origin*sm, n.s.
#also origin and sm alone within one location
#there is no evidence that either influences gross production
production <- aov(umolPg^0.26 ~ sm,
                          data = CPdat)
Anova(production, type="III")
par(mfrow=c(2,2))
plot(production)
dev.off()

###
#consumption
###
hist(CPdat$umolCg)
qqnorm(CPdat$umolCg);qqline(CPdat$umolCg)
transformTukey(CPdat$umolCg,
               start = -10, end = 10, int = 0.01,
               plotit = TRUE, verbose = FALSE, statistic = 1)
#lamba = -0.58
hist(-1 * (CPdat$umolCg)^-0.58)
qqnorm(-1 * (CPdat$umolCg)^-0.58);qqline(-1 * (CPdat$umolCg)^-0.58)

#C analysis
###Looked at Origin*sm, n.s.
#also origin and sm alone within one location
#there is no evidence that either influences gross production
consumption <- aov((-1 * (umolCg)^-0.58) ~ Location,
               data = CPdat)
Anova(consumption, type="III")
par(mfrow=c(2,2))
plot(consumption)
dev.off()

