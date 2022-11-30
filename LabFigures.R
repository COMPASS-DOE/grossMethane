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

Olabs <- c("lowland", "midslope", "upland", "midstream", "upstream")
Llabs <- c("lowland", "midslope", "upland")
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
ggplot(data = na.omit(allData), aes(sm, C_rate)) +
    geom_jitter(aes(color = Location, size = (P_rate), shape = Location)) +
    scale_shape_discrete(labels = c("lowland", "upland")) +
    scale_color_discrete(labels = c("lowland", "upland")) +
    scale_size_continuous("Gross Production") +
    labs(x= "soil water content
       (H\u2082O * g\uFE63\u00b9 dry soil)",
         y= "Gross Consumption
       (\u00b5mol CH\u2084 * g\uFE63\u00b9 dry soil * d\uFE63\u00b9)") +
    guides(shape = guide_legend(override.aes = list(size = 5))) +
    theme(text = element_text(size = 18))

#gross production vs soil moisture
ggplot(data = allData[allData$round == "T1",], aes(sm, P_rate)) +
  geom_point(aes(color = Origin), size = 3) +
  scale_color_discrete(labels = Olabs) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label..,
                                 ..rr.label..,
                                 sep = "~~~")))

#initial gross consumption vs soil moisture
ggplot(data = allData[allData$round == "T1",], aes(sm, C_rate)) +
    geom_point(aes(color = Origin), size = 3) +
    scale_color_discrete(labels = Olabs) +
    geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
    stat_poly_eq(formula = y ~ x,
                 aes(label = paste(..eq.label..,
                                   ..rr.label..,
                                   sep = "~~~")))

#gross consumption vs gross production
#superscripted minus sign is actually as small hyphen
ggplot(data = allData, aes(C_rate, P_rate), shape = Location) +
    geom_jitter(aes(color = sm), size = 5) +
    scale_shape_discrete(labels = c("lowland", "upland")) +
    scale_color_distiller("H\u2082O * g\uFE63\u00b9 dry soil") +
    geom_abline(slope = 1) + lims(x = c(0,0.15), y = c(0,0.15)) +
    geom_smooth(method = lm, formula = y ~ x, se = FALSE, size = 0.75) +
    labs(x=  "Gross Consumption
       (\u00b5mol CH\u2084 * g\uFE63\u00b9 dry soil * d\uFE63\u00b9)",
         y= "Gross Production
       (\u00b5mol CH\u2084 * g\uFE63\u00b9 dry soil * d\uFE63\u00b9)") +
    theme_bw() +
    theme(text = element_text(size = 15), legend.position = "right")


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
