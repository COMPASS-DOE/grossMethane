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
dataRTA %>%
  filter(round == "T1",
         ap_cor > 0.5) %>%
  select(id, round,
         Location, Origin,
         P_rate, C_rate,
         k, sm, delM,
         FCH4, FCO2,
         TA, TS, SWC) -> CheckIt

#fragile code here!
#are data normally distributed?
# par(mfrow = c(4,4))
# for (x in 5:12) {
#     var = colnames(CheckIt)[x]
#     i <- CheckIt[,x]
#     hist(i, main = paste(var))
# }

#fluxes should be transformed

par(mfrow = c(1,1))

###
#production
###
hist(CheckIt$P_rate)
qqnorm(CheckIt$P_rate);qqline(CheckIt$P_rate)
transformTukey(CheckIt$P_rate,
               start = -10, end = 10, int = 0.01,
               plotit = TRUE, verbose = FALSE, statistic = 1)
#lamba = -0.19
hist(-1*CheckIt$P_rate^-0.19)
qqnorm(-1*CheckIt$P_rate^-0.19);qqline(-1*CheckIt$P_rate^-0.19)

#P analysis
#Location was significant as sole variable,
#Until low ap_cor values removed :(((
production1 <- aov(-1*P_rate^-0.19 ~ Location*Origin + sm,
                          data = CheckIt)
Anova(production1)
production2 <- aov(-1*P_rate^-0.19 ~ Origin,
                   data = CheckIt[CheckIt$Location == "g_low",])
Anova(production2)
par(mfrow=c(2,2))
plot(production2)
dev.off()

###
#consumption
###
hist(CheckIt$C_rate)
qqnorm(CheckIt$C_rate);qqline(CheckIt$C_rate)
transformTukey(CheckIt$C_rate,
               start = -10, end = 10, int = 0.01,
               plotit = TRUE, verbose = FALSE, statistic = 1)
#lamba = -0.19
hist(-1*CheckIt$C_rate^-0.93)
qqnorm(-1*CheckIt$C_rate^-0.93);qqline(-1*CheckIt$C_rate^-0.93)

#C analysis
#there is no evidence that either O or L influences gross production
consumption1 <- aov(-1*C_rate^-0.93 ~ Location*Origin + sm,
               data = CheckIt)
Anova(consumption1)
#but there is that soil moisture does!
consumption2 <- aov(-1*C_rate^-0.93 ~ sm,
                    data = CheckIt)
Anova(consumption2)
par(mfrow=c(2,2))
plot(consumption2)
dev.off()

#against each other
gross_rates1 <- aov(-1*P_rate^-0.19 ~ C_rate + sm,
                   data = CheckIt)
Anova(gross_rates1)

gross_rates1 <- aov(-1*C_rate^-0.93 ~ P_rate + sm,
                    data = CheckIt)
Anova(gross_rates1)
