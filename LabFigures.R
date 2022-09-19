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

paired <- read.csv("PairedData.csv")
incdat <- read.csv("2022IncDat.csv")

incdat$Collar <- as.character(incdat$id)
paired$Collar <- as.character(paired$Collar)

#calculate methane totals and net changes
incdat %>%
    select(-AP_pred5, -AP_pred, -notes, -vol,
           -X, -Timestamp, -Timestamp_adj, -id) %>%
    pivot_wider(id_cols = Collar, names_from = round,
                values_from = c(HR.12CH4.Mean,
                                HR.13CH4.Mean,
                                cal12CH4ml,
                                cal13CH4ml,
                                AP_obs)) %>%
    mutate(net = (cal12CH4ml_T4 + cal13CH4ml_T4) - (cal12CH4ml_T0 + cal13CH4ml_T0),
           intial = (cal12CH4ml_T0 + cal13CH4ml_T0),
           final = (cal12CH4ml_T4 + cal13CH4ml_T4),
           APintial = AP_obs_T0,
           APfinal = AP_obs_T4) %>%
    select(Collar, net, intial, final, APintial, APfinal) -> keep


#make incubation data with Collar id's
incdat %>%
    select(-id, -AP_pred5, -AP_pred, -notes, -vol,
           -X, -Timestamp, -Timestamp_adj) %>%
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
    facet_grid(Origin ~ ., labeller = as_labeller(labellies))

#accumulation of methane over incubation
#need adjustment for dilution!
#headspace and sample
#lowland only
ggplot(data = allData[allData$Location != "g_up",],
       aes(round, (cal13CH4ml + cal12CH4ml), colour = Origin)) +
    geom_point(size = 3) +
    geom_line(aes(group = Collar)) +
    ylab("ppm CH4") + xlab("Sampling Time") +
    scale_color_discrete(labels = Olabs)


#change in 13C signature over incubation
ggplot(data = allData[allData$Collar != 52,],
       aes(round, AP_obs, color = sm)) +
    geom_point(aes(shape = Location), size = 3) +
    geom_line(aes(group = Collar)) +
    ylim(0.5,2) +
    ylab("Atom Percent 13C") + xlab("Sampling Time") +
    scale_color_continuous(name = "Soil Moisture") +
    scale_shape_discrete(labels = c("lowland",
                                    "upslope")) +
    facet_grid(Origin ~ ., labeller = as_labeller(labellies))



#drop redundant rows from long form data
allData %>%
    select(Location, Origin, Collar,
           umolKg, umolPg, sm, FCH4,
           net, k0) %>%
    unique() -> graph

#summary of net incubation
ggplot(graph[graph$Collar != 52,],
       aes(Location, net, fill = Origin)) +
    scale_x_discrete(labels = c("lowland",
                                "upslope")) +
    scale_fill_discrete(labels = Olabs) +
    geom_boxplot()

