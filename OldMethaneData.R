
#This code is to read-in and reanalyze
#2019 and 2020 soil methane field data
#from PREMIS
#see Hopple et al 2022 for more detail:
#https://doi.org/10.1016/j.soilbio.2022.108675

#created 20 September 2022
#by:Kendalynn Morris

library(ggplot2)
library(ggpmisc)
library(dplyr)
library(tidyr)
library(lubridate)

Olabs <- c("lowland", "midslope", "upslope", "midstream", "upstream")
Llabs <- c("lowland", "midslope", "upslope")
labellies <- c('g_low' = "lowland",
               'g_mid' = "midslope",
               'g_up' = "upslope",
               'midstream' = "midstream",
               'upstream' = "upstream")

#read in collar ids and data
collars <- read.csv("field-measurements/cores_collars.csv")
data_raw19 <- read.csv("2019_CH4data.csv")
data_raw20 <- read.csv("2020_CH4data.csv")

data_raw <- bind_rows(data_raw19, data_raw20)

#format timestamp
data_raw$timestamp <- mdy_hm(data_raw$datetime, tz="EST")

#filter bad values and format
data_raw %>%
  filter(FCH4 != -9999) %>%
  mutate(Collar = as.character(Collar),
         date = date(timestamp)) -> data_raw

#format Collar
collars$Collar <- as.character(collars$Collar)

#join experiment ids and data
collars %>%
  select(Plot, Location, Collar, Experiment) %>%
  relocate(Collar, .before = Plot) %>%
  right_join(data_raw, collars,
            by = "Collar") -> data


#Fill in NA's in reps with 1
data[is.na(data$Rep),]$Rep <- 1

#rename Plot as Origin
data %>%
  rename("Origin" = "Plot") -> data

#give origins easier names
data$Origin <- recode_factor(data$Origin,
                             "HSLE" = "g_low",
                             "HSME" = "g_mid",
                             "HSHE" = "g_up",
                             "MSLE" = "midstream",
                             "LSLE" = "upstream")

#labelled column with current collar location in the same way
data$Location <- recode_factor(data$Location,
                               "HSLE" = "g_low",
                               "HSME" = "g_mid",
                               "HSHE" = "g_up",
                               "MSLE" = "midstream",
                               "LSLE" = "upstream")

#summarize within sampling sessions

#summarize by date and rep
data %>%
  group_by(date, Collar, Rep, Origin,
           Location, Experiment) %>%
  mutate_at(c("ts_2", "SWC"), ~na_if(., 0)) %>%
  summarise(timestamp = mean(timestamp),
            FCH4 = median(FCH4),
            SWC = mean(SWC),
            TS = mean(ts_2)) %>%
  ungroup() %>%
  filter(! Location %in% c("MSME", "MSHE", "MSLE",
                           "LSME", "LSHE", "LSLE",
                           "upstream", "midstream"),
         SWC > 0) %>%
  relocate(date, .after = Rep) %>%
  mutate(month = as.factor(month(date)),
         day = yday(timestamp)) -> old_dat

old_dat$month <- recode_factor(old_dat$month,
                         "11" = "Nov",
                        "12" = "Dec",
                        "1" = "Jan",
                        "2" = "Feb",
                        "3" = "March",
                        "4" = "April",
                        "5" = "May",
                        "7" = "July")

ggplot(old_dat, aes(month, SWC, fill = Origin)) +
  geom_boxplot(aes(group = interaction(month, Origin))) +
  scale_fill_discrete(name = "Soil Origin",
                      labels = Olabs) +
  facet_wrap(Location~., scales="free") +
  theme_bw()

ggplot(old_dat, aes(month, FCH4, fill = Origin)) +
  geom_boxplot(aes(group = interaction(month(timestamp), Origin))) +
  scale_fill_discrete(name = "Soil Origin",
                      labels = Olabs) +
  facet_wrap(Location ~ ., scales = "free") +
    theme_bw()

ggplot(old_dat, aes(SWC, FCH4, color = as.factor(Origin))) +
  geom_point(size = 3) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..p.value.label..,
                                 ..rr.label..,
                                 sep = "~~~"))) +
  scale_color_discrete(name = "Soil Origin",
                       labels = Olabs) +
  theme_bw()

ggplot(old_dat, aes(SWC, FCH4)) +
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

ggplot(old_dat, aes(TS, FCH4)) +
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

ggplot(old_dat, aes(Location, FCH4, fill = Origin)) +
  scale_fill_discrete(name = "Soil Origin",
                      labels = Olabs) +
  scale_x_discrete(labels = Llabs) +
  geom_boxplot() + theme_bw()

ggplot(old_dat, aes(Origin, FCH4, fill = Location)) +
  scale_fill_discrete(name = "Plot Location",
                      labels = Llabs) +
  scale_x_discrete(labels = Olabs) +
  geom_boxplot() + theme_bw()

ggplot(old_dat, aes(Origin, FCH4, fill = Origin)) +
  scale_x_discrete(name = "Soil Origin",
                      labels = Olabs) +
  geom_boxplot() + theme_bw()
#
#methane 2019 and 2020 versions of Hopple.et.al 2022 graphs
#

old_dat %>% filter(Location == "g_up",
                 Origin == "g_up") -> upland
old_dat %>% filter(Location == "g_low",
                 Origin %in% c("g_up", "g_low")) -> lowland

upland$Movement <- "upland-upland"
lowland$Movement <- "upland-lowland"

lowland[lowland$Origin == "g_low",]$Movement <- "lowland-lowland"
upland %>% bind_rows(lowland) -> uplow

ggplot(data = uplow, aes(month(timestamp), FCH4)) +
  geom_boxplot(alpha = 0.2, width = 0.4, aes(color = Movement, group = interaction(Movement, week(timestamp)), fill = Movement)) +
  geom_smooth(aes(color = Movement), se = FALSE) +
  labs(x = "Month", y = "Methane Flux") +
  scale_fill_manual(values = c("lowland-lowland" = "#f73bac",
                               "upland-lowland" = "#cb81e6", "upland-upland" = "#19cf9b")) +
  scale_color_manual(values = c("lowland-lowland" = "#f73bac",
                                "upland-lowland" = "#9b42f5", "upland-upland" = "#2eab5e")) +
  theme_bw()

ggplot(data = uplow, aes(Movement, FCH4, fill = Movement)) +
  geom_boxplot(alpha = 0.5, aes(color = Movement)) +
  labs(x = "Movement", y = "Flux CH4") +
  scale_fill_manual(values = c("lowland-lowland" = "#f73bac",
                               "upland-lowland" = "#cb81e6", "upland-upland" = "#19cf9b")) +
  scale_color_manual(values = c("lowland-lowland" = "#f73bac",
                                "upland-lowland" = "#9b42f5", "upland-upland" = "#2eab5e")) +
  theme_bw()


library(nlme)
old_dat %>%
  ungroup() %>%
  mutate(type = paste(Location, Origin)) -> old_dat

CH4lme <- lme(FCH4 ~ as.numeric(day) + SWC + type,
                     random = ~ 1|Collar,
                     data = old_dat,
                     weights = varExp(form = ~as.numeric(day)),
                     na.action = na.omit,
                     method = "ML")

summary(CH4lme)

old_dat %>%
  filter(month == "Nov") -> Nov_old_dat


CH4lme_Nov <- lme(FCH4 ~ as.numeric(day) + SWC + type,
              random = ~ 1|Collar,
              data = Nov_old_dat,
              weights = varExp(form = ~as.numeric(day)),
              na.action = na.omit,
              method = "ML")


summary(CH4lme_Nov)
plot_model(CH4lme_Nov)
