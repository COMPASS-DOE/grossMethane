
# This code is for graphing flux data from GCREW.
# for data from PairedMethaneLiCorData.R

# Created September 2022
# Kendal

library(lubridate)
library(dplyr)
library(ggplot2)
library(ggpmisc)

f_dat <- read.csv("licorRTA.csv")
f_dat$date <- as.Date(f_dat$date, "%m/%d/%Y")
f_dat$timestamp <- mdy_hm(f_dat$timestamp, tz="EST")
#remove 0's from SWC
#values from May 11th very low
#best would be to remove just
f_dat[f_dat$date == "2022-05-11",]$SWC <- 0
is.na(f_dat$SWC) <- !f_dat$SWC

Olabs <- c("lowland", "midslope", "upslope", "midstream", "upstream")
Llabs <- c("lowland", "midslope", "upslope")
months <- c("May", "June", "July", "August")

#explore relationship between SWC and g_mid origin soils in lowland

f_dat %>%
    filter(Location == "g_low") -> g_low

ggplot(g_low, aes(month(timestamp), SWC, fill = Origin)) +
    geom_boxplot(aes(group = interaction(month(timestamp), Origin))) +
    scale_fill_discrete(name = "Soil Origin",
                         labels = Olabs) +
    scale_x_continuous(breaks = c(5, 6, 7, 8),
                       labels = months,
                       name = "Month") +
    theme_bw()

ggplot(g_low, aes(SWC, FCH4, color = as.factor(Origin))) +
    geom_point(size = 3) +
    geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
    stat_poly_eq(formula = y ~ x,
                 aes(label = paste(..p.value.label..,
                                   ..rr.label..,
                                   sep = "~~~"))) +
    scale_color_discrete(name = "Soil Origin",
                         labels = Olabs) +
    theme_bw()

ggplot(g_low, aes(SWC, FCH4)) +
    geom_point(aes(color = as.factor(Origin))) +
    geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
    stat_poly_eq(formula = y ~ x,
                 aes(label = paste(..p.value.label..,
                                   ..eq.label..,
                                   ..rr.label..,
                                   sep = "~~~"))) +
    scale_color_discrete(name = "Soil Origin",
                         labels = Olabs) +
    theme_bw()


ggplot(f_dat[f_dat$FCH4 < 50 &
                 f_dat$Location != "g_mid",], aes(Location, FCH4, fill = Origin)) +
    scale_fill_discrete(name = "Soil Origin",
                        labels = Olabs) +
  geom_hline(yintercept = 0, color = "red", linetype = 2) +
    scale_x_discrete(labels = c("lowland", "upland")) +
      geom_boxplot() + theme_bw() +
   theme(text=element_text(size=12))

#
#summer 2022 versions of Hopple.et.al 2022 graphs
#

f_dat %>% filter(Location == "g_up",
                Origin == "g_up") -> upland
f_dat %>% filter(Location == "g_low",
                Origin %in% c("g_up", "g_low")) -> lowland

upland$Movement <- "upland-upland"
lowland$Movement <- "upland-lowland"

lowland[lowland$Origin == "g_low",]$Movement <- "lowland-lowland"
upland %>% bind_rows(lowland) -> uplow

#Hopple comparison graph A (CO2)
ggplot(data = uplow, aes(week(timestamp), FCO2)) +
  geom_boxplot(alpha = 0.2, width = 0.4, aes(color = Movement, group = interaction(Movement, week(timestamp)), fill = Movement)) +
  geom_smooth(aes(color = Movement), se = FALSE) +
  labs(x = "Calendar Week", y = "Flux CO2") +
  scale_fill_manual(values = c("lowland-lowland" = "#f73bac",
                               "upland-lowland" = "#cb81e6", "upland-upland" = "#19cf9b")) +
  scale_color_manual(values = c("lowland-lowland" = "#f73bac",
                                "upland-lowland" = "#9b42f5", "upland-upland" = "#2eab5e")) +
  theme_bw()
#ggsave("CO2 flux Hopple Model.png", width = 12.5, height = 4)

#2022 methane version
ggplot(data = uplow[uplow$FCH4 < 150,], aes(week(timestamp), FCH4)) +
    geom_boxplot(alpha = 0.2, width = 0.4, aes(color = Movement, group = interaction(Movement, week(timestamp)), fill = Movement)) +
    geom_smooth(aes(color = Movement), se = FALSE) +
    labs(x = "Calendar Week", y = "Flux CH4") +
    scale_fill_manual(values = c("lowland-lowland" = "#f73bac",
                                 "upland-lowland" = "#cb81e6", "upland-upland" = "#19cf9b")) +
    scale_color_manual(values = c("lowland-lowland" = "#f73bac",
                                  "upland-lowland" = "#9b42f5", "upland-upland" = "#2eab5e")) +
    theme_bw()



# Hopple comparison graph B (CO2)
ggplot(data = uplow, aes(Movement, FCO2, fill = Movement)) +
  geom_boxplot(alpha = 0.5, aes(color = Movement)) +
  labs(x = "Movement", y = "Flux CO2") +
  scale_fill_manual(values = c("lowland-lowland" = "#f73bac",
                               "upland-lowland" = "#cb81e6", "upland-upland" = "#19cf9b")) +
  scale_color_manual(values = c("lowland-lowland" = "#f73bac",
                                "upland-lowland" = "#9b42f5", "upland-upland" = "#2eab5e")) +
  theme_bw()

#2022 methane version
ggplot(data = uplow[uplow$FCH4 < 150,], aes(Movement, FCH4, fill = Movement)) +
    geom_boxplot(alpha = 0.5, aes(color = Movement)) +
    labs(x = "Movement", y = "Flux CH4") +
    scale_fill_manual(values = c("lowland-lowland" = "#f73bac",
                                 "upland-lowland" = "#cb81e6", "upland-upland" = "#19cf9b")) +
    scale_color_manual(values = c("lowland-lowland" = "#f73bac",
                                  "upland-lowland" = "#9b42f5", "upland-upland" = "#2eab5e")) +
    theme_bw()



#Summaries
TS_stats <- f_dat %>% group_by(Origin, Location) %>%
  summarize(sdev = sd(TS),
            minimun = min(TS),
            maximum = max(TS),
            mean = mean(TS),
            med = median(TS)) %>%
  mutate(variable = "TS")

SWC_stats <- f_dat %>% group_by(Location, date) %>%
  summarize(mean = mean(SWC, na.rm = TRUE),
            med = median(SWC,na.rm = TRUE),
            sdev = sd(SWC, na.rm = TRUE),
            minimun = min(SWC, na.rm = TRUE),
            maximum = max(SWC, na.rm = TRUE)
            ) %>%
  mutate(variable = "SWC")

FCH4_stats <- f_dat %>% group_by(Origin, Location) %>%
  summarize(sdev = sd(FCH4),
            minimun = min(FCH4),
            maximum = max(FCH4),
            mean = mean(FCH4),
            med = median(FCH4)) %>%
  mutate(variable = "FCH4")

FCO2_stats <- f_dat %>% group_by(Origin, Location) %>%
  summarize(sdev = sd(FCO2),
            minimun = min(FCO2),
            maximum = max(FCO2),
            mean = mean(FCO2),
            med = median(FCO2)) %>%
  mutate(variable = "FCO2")




#example graphs of Mitchell's
f_dat %>%
  dplyr::filter(Origin %in% c('g_low','g_mid','g_up'),
                week(timestamp) == 19) %>%
  ggplot(aes(week(timestamp), FCO2, fill = Location)) +
  geom_boxplot(aes(group = interaction(week(timestamp), Location))) +
  facet_grid(.~Origin, labeller = labeller(Origin = origin.labs)) +
  labs(x = "Calendar Week", y = "Flux CO2", title = "Interaction of Origin & Location on CO2") +
  theme_bw()

f_dat %>%
  dplyr::filter( FCH4 < 400 & FCH4 >-5,
                 Origin %in% c('g_low','g_mid','g_up')) %>%
  ggplot(aes(date(timestamp), FCH4, color = SWC)) +
  scale_color_distiller(direction = 1) +
  geom_point() +
  labs(x = "Date", y = "Flux CH4", color = "Soil Moisture") +
  facet_wrap(~Origin) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
