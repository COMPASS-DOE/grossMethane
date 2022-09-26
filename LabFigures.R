# This code is for graphing
# gross methane fluxes from GCREW soils.
# 13C tracer incubations were conducted the last week of July 22
# in wetland edge (aka lowland aka transition) and upland soils

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

#gross production vs consumption
ggplot(data = paired, aes(umolPg, umolKg, colour = sm)) +
  geom_point(size = 3) +
  geom_smooth(method = lm, formula = y ~ x) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label..,
                                 ..rr.label..,
                                 sep = "~~~")))

#gross production vs soil moisutre
ggplot(data = paired, aes(sm, umolPg)) +
  geom_point(aes(color = Origin), size = 3) +
  scale_color_discrete(labels = Olabs) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..p.value.label..,
                                 ..eq.label..,
                                 ..rr.label..,
                                 sep = "~~~")))

#gross consumption vs soil moisture
ggplot(data = paired, aes(sm, umolKg, colour = sm)) +
  geom_point(size = 3) +
  facet_grid(Location ~ .) +
  geom_smooth(method = lm, formula = y ~ x) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..p.value.label..,
                                 ..eq.label..,
                                 ..rr.label..,
                                 sep = "~~~")))

#comparing field and lab soil moisture
#needs unit conversion!
ggplot(data = paired,
       aes(sm, SWC, color = Collar)) +
  geom_point(size = 3)

#comparing field and lab flux
#needs unit conversion!
ggplot(data = paired,
       aes(FCH4, umolPg, color = Collar)) +
  geom_point(size = 3)

#accumulation of methane over incubation
#need adjustment for dilution!
#headspace and sample
ggplot(data = allData[allData$Collar != 52,],
       aes(round, (cal13CH4ml + cal12CH4ml), color = sm)) +
    geom_point(aes(shape = Location), size = 3) +
    geom_line(aes(group = Collar)) +
    ylab("ppm CH4") + xlab("Sampling Time") +
    scale_color_continuous(name = "Soil Moisture") +
    scale_shape_discrete(labels = c("lowland",
                                    "upslope")) +
    facet_wrap(Origin ~ ., labeller = as_labeller(labellies))

#accumulation of methane over incubation
#need adjustment for dilution!
#headspace and sample
ggplot(data = allData, #[allData$Location != "g_up",],
       aes(round, (cal13CH4ml + cal12CH4ml), colour = Origin)) +
    geom_point(size = 3) +
    geom_line(aes(group = Collar)) +
    ylab("ppm CH4") + xlab("Sampling Time") +
    scale_color_discrete(labels = Olabs)


#change in 13C signature over incubation
ggplot(data = allData, #[allData$Collar != 52,],
       aes(round, HR.Delta.iCH4.Mean, color = sm)) +
    geom_point(aes(shape = Location), size = 3) +
    geom_line(aes(group = Collar)) +
    ylab("per mil") + xlab("Sampling Time") +
    scale_color_continuous(name = "Soil Moisture") +
    scale_shape_discrete(labels = c("lowland",
                                    "upslope")) +
    facet_wrap(Origin ~ ., labeller = as_labeller(labellies))


#drop redundant rows from long form data
allData %>%
    select(Location, Origin, Collar,
           umolKg, umolPg, sm,
           mass, FCH4, dCH4_f,
           dCH4_i, net, k0) %>%
    unique() -> graph

#summary of net incubation
ggplot(graph[graph$Collar != 52,],
       aes(Location, net, fill = Origin)) +
    scale_x_discrete(labels = c("lowland",
                                "upslope")) +
    scale_fill_discrete(labels = Olabs) +
    geom_boxplot()

#summary of production
ggplot(graph,
       aes(Location, umolPg, fill = Origin)) +
    scale_x_discrete(labels = c("lowland",
                                "upslope")) +
    scale_fill_discrete(labels = Olabs) +
    geom_boxplot()

#summary of net rates
ggplot(graph,
       aes(Location, umolPg+umolKg, fill = Origin)) +
    scale_x_discrete(labels = c("lowland",
                                "upslope")) +
    scale_fill_discrete(labels = Olabs) +
    geom_boxplot()


#summary of terminal delta values
ggplot(graph,
       aes(Collar, dCH4_f,  color = Origin)) +
    facet_wrap(. ~ Location, scales = "free",
               labeller = as_labeller(labellies)) +
    scale_color_discrete(labels = Olabs) +
    geom_point(aes(size = sm)) + theme_bw()

