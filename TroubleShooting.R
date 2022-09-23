# This code is for figuring out
# what the heck is going on
# with this flux data

# September 2022
# Kendalynn A. Morris

#read gross rate results generated on September 13th
#from commit "venture into toyland"

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpmisc)
library(car)

Olabs <- c("lowland", "midslope", "upslope", "midstream", "upstream")
Llabs <- c("lowland", "midslope", "upslope")
labellies <- c('g_low' = "lowland",
               'g_mid' = "midslope",
               'g_up' = "upslope",
               'midstream' = "midstream",
               'upstream' = "upstream")

deltas <- read.csv("incDeltas.csv")
incdat <- read.csv("2022IncDat.csv")
paired <- read.csv("PairedData.csv")

deltas$Collar <- as.character(deltas$id)
incdat$Collar <- as.character(incdat$id)
paired$Collar <- as.character(paired$Collar)

deltas %>%
  select(-X, -id, -Timestamp,
         -HR.12CH4.Mean,
         -HR.13CH4.Mean,) -> deltas2add

#calculate methane totals and net changes
incdat %>%
  select(-AP_pred5, -AP_pred, -notes, -vol,
         -X, -Timestamp, -Timestamp_adj, -id) %>%
  left_join(deltas2add, by = c("Collar" = "Collar",
                               "round" = "round")) %>%
  pivot_wider(id_cols = Collar, names_from = round,
              values_from = c(HR.12CH4.Mean,
                              HR.13CH4.Mean,
                              HR.Delta.iCH4.Mean,
                              Delta.13CO2.Mean,
                              cal12CH4ml,
                              cal13CH4ml,
                              AP_obs)) %>%
  mutate(net = (cal12CH4ml_T4 + cal13CH4ml_T4) - (cal12CH4ml_T0 + cal13CH4ml_T0),
         intial = (cal12CH4ml_T0 + cal13CH4ml_T0),
         final = (cal12CH4ml_T4 + cal13CH4ml_T4),
         dCH4_i = HR.Delta.iCH4.Mean_T0,
         dCH4_f = HR.Delta.iCH4.Mean_T4,
         dCO2_i = Delta.13CO2.Mean_T0,
         dCO2_f = Delta.13CO2.Mean_T4,
         APintial = AP_obs_T0,
         APfinal = AP_obs_T4) %>%
  select(Collar, net, intial, final, dCH4_i, dCH4_f,
         dCO2_i, dCO2_f, APintial, APfinal) -> keep


#make incubation data with Collar id's
incdat %>%
  select(-id, -AP_pred5, -AP_pred, -notes, -vol,
         -X, -Timestamp, -Timestamp_adj) %>%
  left_join(deltas2add, by = c("Collar" = "Collar",
                               "round" = "round")) %>%
  left_join(keep, by = "Collar") %>%
  left_join(paired, by = "Collar") %>%
  select(-X) %>%
  relocate(Collar, .before = round) %>%
  relocate(Origin, .after = Collar) %>%
  relocate(Location, .after = Origin) -> allData

#drop redundant rows from long form data
allData %>%
  select(Location, Origin, Collar,
         umolKg, umolPg, sm, FCH4,
         net, k0) %>%
  distinct() -> graph

#summary of net rates
#all but 52 should be below 0!!!
ggplot(graph,
       aes(Location, umolPg+umolKg, fill = Origin)) +
  scale_x_discrete(labels = c("lowland",
                              "upslope")) +
  scale_fill_discrete(labels = Olabs) +
  geom_boxplot()

#two distinct issues
allData$trouble <- "none"
allData$netGross <- allData$P - abs(allData$k)
allData[allData$netGross > 0,]$trouble <- "highP"
allData[allData$k == 0.0000100,]$trouble <- "lowk"

ggplot(allData[allData$trouble != "none",], aes(round, `HR.Delta.iCH4.Mean`)) +
  geom_point(aes(shape = Location, color = Collar, size = `HR.Delta.iCH4.Std`)) +
  geom_line(aes(group = Collar, color = Collar)) +
  facet_grid(trouble~Origin)

