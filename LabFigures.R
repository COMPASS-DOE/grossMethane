# This code is for graphing
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

incdat <- read.csv("2022IncDat.csv")

incdat %>%
    select(-AP_pred5, -AP_pred, -notes, -vol,
           -X, -Timestamp, -Timestamp_adj) %>%
    pivot_wider(id_cols = id, names_from = round,
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
    select(id, net, intial, final, APintial, APfinal) -> keep

keep$id <- as.character(keep$id)
incdat$id <- as.character(incdat$id)
data$Collar <- as.character(data$Collar)

incdat %>%
    select(-AP_pred5, -AP_pred, -notes, -vol,
           -X, -Timestamp, -Timestamp_adj) %>%
    left_join(keep, by = "id") %>%
    mutate(Collar = id) %>%
    left_join(data, by = "Collar") -> allDat





ggplot(data = data, aes(umolPg, umolKg, colour = sm)) +
  geom_point(size = 3) +
  geom_smooth(method = lm, formula = y ~ x) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label..,
                                 ..rr.label..,
                                 sep = "~~~")))

ggplot(data = data, aes(sm, umolPg)) +
  geom_point(aes(color = Origin), size = 3) +
  scale_color_discrete(labels = Olabs) +
  #facet_grid(Location ~ .) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..p.value.label..,
                                 ..eq.label..,
                                 ..rr.label..,
                                 sep = "~~~")))

ggplot(data = data, aes(sm, umolKg, colour = sm)) +
  geom_point(size = 3) +
  facet_grid(Location ~ .) +
  geom_smooth(method = lm, formula = y ~ x) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..p.value.label..,
                                 ..eq.label..,
                                 ..rr.label..,
                                 sep = "~~~")))

ggplot(data = data,
       aes(sm, SWC, colour = as.factor(Collar))) +
  geom_point(size = 3)

ggplot(data = data,
       aes(FCH4, umolPg, colour = as.factor(Collar))) +
  geom_point(size = 3)

ggplot(data = allDat[allDat$Collar != 52,],
       aes(round, (cal13CH4ml + cal12CH4ml), colour = sm)) +
    geom_point(aes(shape = Location), size = 3) +
    geom_line(aes(group = Collar)) +
    ylab("ppm CH4") + xlab("Sampling Time") +
    scale_color_continuous(name = "Soil Moisture") +
    scale_shape_discrete(labels = c("lowland",
                                    "upslope")) +
    facet_grid(Origin ~ ., labeller = as_labeller(seriously))

ggplot(data = allDat[allDat$Collar != 52,],
       aes(round, AP_obs, colour = sm)) +
    geom_point(aes(shape = Location), size = 3) +
    geom_line(aes(group = Collar)) +
    ylim(0.75,1.75) +
    ylab("Atom Percent 13C") + xlab("Sampling Time") +
    scale_color_continuous(name = "Soil Moisture") +
    scale_shape_discrete(labels = c("lowland",
                                    "upslope")) +
    facet_grid(Origin ~ ., labeller = as_labeller(seriously))

ggplot(data = allDat[allDat$Location != "g_up",],
       aes(round, (cal13CH4ml + cal12CH4ml), colour = Origin)) +
    geom_point(size = 3) +
    geom_line(aes(group = Collar)) +
    ylab("ppm CH4") + xlab("Sampling Time") +
    scale_color_discrete(labels = Olabs)

seriously <- c('g_low' = "lowland",
               'g_mid' = "midslope",
               'g_up' = "upslope",
               'midstream' = "midstream",
               'upstream' = "upstream")

ggplot(data = allDat[allDat$Collar != 52,],
       aes(round, AP_obs, colour = sm)) +
           geom_point(size = 3) +
           geom_line(aes(group = Collar)) +
           facet_grid(Location ~ Origin)
allDat %>%
    select(Location, Origin, Collar,
           umolKg, umolPg, sm, FCH4,
           net, k0) %>%
    unique() -> graph

ggplot(graph[graph$Collar != 52,],
       aes(Location, net, fill = Origin)) +
    scale_x_discrete(labels = c("lowland",
                                "upslope")) +
    scale_fill_discrete(labels = Olabs) +
    geom_boxplot()

