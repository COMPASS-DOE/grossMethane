# This code is for graphing
# gross methane fluxes from GCREW soils.
# 13C tracer incubations were conducted the last week of July 22
# in wetland edge (aka lowland aka transition) and upland soils

# October 2022
# Kendalynn A. Morris

library(car)
library(dplyr)
library(emojifont)
library(lubridate)
library(ggplot2)
library(ggpmisc)
library(tidyr)
library(readr)

Olabs <- c("lowland", "midslope", "upland", "midstream", "upstream")
Llabs <- c("lowland", "upland")
labellies <- c('g_low' = "lowland",
               'g_mid' = "midslope",
               'g_up' = "upland",
               'midstream' = "midstream",
               'upstream' = "upstream")

data <- read_csv("PairedData.csv",
                 col_types = "dccccdddddddddddddddddddddddddddddddddddcdcdddddc")


#calculate methane totals and net changes
#ignoring T5 data for now
data %>%
    select(-`...1`) %>%
    filter(round != "T5") %>%
    pivot_wider(id_cols = id, names_from = round,
                values_from = c(delC,
                                delCstd,
                                delM,
                                delMstd,
                                Mppm,
                                M13ppm,
                                Cppm,
                                C13ppm,
                                AP_obs)) %>% #, AP_pred)) %>% Not currently used
    mutate(intial = (Mppm_T0 + M13ppm_T0),
           final = (Mppm_T4 + M13ppm_T4),
           net = final - intial,
           intialC = (Cppm_T0 + C13ppm_T0),
           finalC = (Cppm_T4 + C13ppm_T4),
           netC = finalC - intialC,
           dCH4_i = delM_T0,
           dCH4_istd = delMstd_T0,
           dCH4_f = delM_T4,
           dCH4_fstd = delMstd_T4,
           dCO2_i = delC_T0,
           dCO2_istd = delCstd_T0,
           dCO2_f = delC_T4,
           dCO2_fstd = delCstd_T4,
           APintial = AP_obs_T0,
           APfinal = AP_obs_T4) %>%
    select(id, intial, final, net, intialC, finalC, netC,
           dCH4_i, dCH4_istd, dCH4_f, dCH4_fstd,
           dCO2_i, dCO2_istd, dCO2_f, dCO2_fstd,
           APintial, APfinal) -> timeDat

#combine
data %>%
    select(-`...1`) %>%
    left_join(timeDat, by = "id") %>%
    mutate(Lab_time = ymd_hms(l_time, tz = "UTC"),
           Field_time = mdy_hm(f_time, tz = "UTC")) %>%
    select(-l_time, -f_time) -> allData



#gross consumption vs soil moisture
#superscripted minus sign is actually as small hyphen
ggplot(data = na.omit(allData), aes(sm, uk)) +
    geom_jitter(aes(color = Location, size = (P_rate), shape = Location)) +
    scale_shape_discrete(labels = c("lowland", "upland")) +
    scale_color_discrete(labels = c("lowland", "upland")) +
    scale_size_continuous("Gross Production") +
    labs(x= "soil water content
       (H\u2082O * g\uFE63\u00b9 dry soil)",
         y= "Consumption (d\uFE63\u00b9)") +
    guides(shape = guide_legend(override.aes = list(size = 5))) +
  theme_minimal(base_size = 15) +
  theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20))

#gross production vs soil moisture
CheckIt %>%
  filter(id != "52") %>%
ggplot(., aes(sm, P_rate)) +
  geom_point(aes(color = Origin, shape = Location), size = 3) +
  scale_color_discrete(name = "Soil Origin",
                       labels = Olabs) +
  scale_shape_discrete(labels = Llabs) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, size = 0.5) +
  labs(x=  "Soil Moisture
       (g\u207B\u00b9 dry soil)",
       y= "Gross Production
       (\u00b5mol CH\u2084 * g\u207B\u00b9 dry soil * d\u207B\u00b9)") +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label..,
                                 ..rr.label..,
                                 sep = "~~~"))) +
  theme_minimal(base_size = 13) +
  theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20))

CheckIt %>%
  filter(id != "52") %>%
  ggplot(., aes(Location, P_rate)) +
  geom_boxplot(aes(fill = Origin)) +
  scale_fill_discrete(name = "Soil Origin",
                       labels = Olabs) +
  scale_x_discrete(labels = Llabs) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, size = 0.5) +
  labs(x=  "Location",
       y= "Gross Production
       (\u00b5mol CH\u2084 * g\u207B\u00b9 dry soil * d\u207B\u00b9)") +
  geom_hline(yintercept = 0, color = "red", linetype = 2) +
  theme_bw(base_size = 13) +
  theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20))

#k vs soil moisture
ggplot(CheckIt, aes(sm, k)) +
    geom_point(aes(color = Origin, shape = Location), size = 3) +
    scale_color_discrete(labels = Olabs) +
    scale_shape_discrete(labels = Llabs) +
    geom_smooth(method = lm, formula = y ~ x, se = FALSE, size = 0.5) +
  labs(x=  "Soil Moisture
       (g\uFE63\u00b9 dry soil)",
       y= "Consumption (d\uFE63\u00b9)") +
    stat_poly_eq(formula = y ~ x,
                 aes(label = paste(..eq.label..,
                                   ..rr.label..,
                                   sep = "~~~"))) +
  theme_bw(base_size = 15) +
  theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20))


#summary of k
ggplot(CheckIt, aes(Location, k)) +
  geom_boxplot(aes(fill = Origin)) +
  scale_color_discrete(labels = Olabs) +
  scale_x_discrete(labels = Llabs) +
  theme_bw(base_size = 15) +
  theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20))

#k vs gross production
#superscripted minus sign is actually as small hyphen
ggplot(data = CheckIt[CheckIt$id != 52,], aes(P_rate, k), shape = Location) +
    geom_jitter(aes(color = sm), size = 5) +
    scale_shape_discrete(labels = c("lowland", "upland")) +
    scale_color_distiller("g * g\uFE63\u00b9") +
    geom_abline(slope = 1, size = 0.25, color = "red", linetype = 2) +
    lims(x = c(0,75), y = c(0,75)) +
    geom_smooth(method = lm, formula = y ~ x, se = FALSE, size = 0.75) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label..,
                                 ..rr.label..,
                                 sep = "~~~")))+
    labs(x=  "Gross Production
       (\u00b5mol CH\u2084 * g\uFE63\u00b9 dry soil * d\uFE63\u00b9)",
         y= "k of consumption (d\uFE63\u00b9)") +
  theme_minimal(base_size = 15) +
  theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20))


#accumulation of methane over incubation
#without our friend 52
ggplot(data = allData[allData$id != 52,],
       aes(round, (Mppm + M13ppm), color = sm)) +
    geom_point(aes(shape = Location), size = 3) +
    geom_line(aes(group = id)) +
    ylab("ppm CH4") + xlab("Sampling Time") +
    scale_color_continuous(name = "Soil Moisture") +
    scale_shape_discrete(labels = c("lowland",
                                    "upland")) +
    facet_wrap(Origin ~ ., labeller = as_labeller(labellies))


#net lab methane
#without our friend 52
allData %>%
  filter(id != "52",
         round == "T1") %>%
ggplot(.,
       aes(Location, net, fill = Origin)) +
  geom_boxplot() +
  ylab("net CH4 (ppm)") + xlab("Location") +
  ylim(-11,0) + geom_hline(yintercept = 0, color = "red", linetype = 2) +
  scale_fill_discrete(labels = Olabs) +
  scale_x_discrete(labels = c("lowland","upland")) +
  theme_bw() + theme(text=element_text(size=12))


#drop redundant rows from long form data
allData %>%
    select(Location, Origin, id,
           C_rate, P_rate, sm,
           mass, FCH4, dCH4_f,
           dCH4_i, net) %>%
    unique() -> graph

#summary of net incubation
ggplot(graph[graph$id != 52,],
       aes(Location, net, fill = Origin)) +
    scale_x_discrete(labels = c("lowland",
                                "upland")) +
    scale_fill_discrete(labels = Olabs) +
    geom_boxplot()

#summary of production
ggplot(graph[graph$id != 52,],
       aes(Location, P_rate, fill = Origin)) +
    scale_x_discrete(labels = c("lowland",
                                "upland")) +
    scale_fill_discrete(labels = Olabs) +
    geom_boxplot()

#summary of net rates
ggplot(graph,
       aes(Location, net, fill = Origin)) +
    scale_x_discrete(labels = c("lowland",
                                "upland")) +
    scale_fill_discrete(labels = Olabs) +
    geom_boxplot()


#summary of terminal delta values
ggplot(graph,
       aes(Location, dCH4_f,  fill = Origin)) +
    scale_x_discrete(labels = c("lowland",
                                "upland")) +
    scale_fill_discrete(labels = Olabs) +
    geom_boxplot() + theme_bw()


#summary of field net rates
ggplot(graph[graph$FCH4 < 360,],
       aes(Origin, FCH4, fill = Origin)) +
    scale_x_discrete(labels = Olabs,
    ) +
    scale_fill_discrete(labels = Olabs) +
    labs(x= " ",
         y= "Net Rate \n (nmol CH\u2084 * m\u207b\u00B2 * s\u207b\u00b9)") +
    theme_bw() + geom_boxplot() +
    facet_grid(Location ~ . , scales = 'free',
               labeller = as_labeller(labellies)) +
    theme(axis.text.x = element_text(angle = 22.5,
                                     hjust = 1))

#comparing field and lab soil moisture
#needs unit conversion!
ggplot(data = allData,
       aes(sm, SWC, color = id)) +
    geom_point(size = 3)

#comparing field and lab flux
#needs unit conversion!
ggplot(data = allData,
       aes(FCH4, P_rate, color = id)) +
    geom_point(size = 3)
