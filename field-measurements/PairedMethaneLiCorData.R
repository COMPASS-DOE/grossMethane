
#This code is to read-in and graph
#2022 soil methane and respiration data
#from the HS (GCREW) transect of the transplant experiment
#see Hopple et al 2022 for more detail:
#https://doi.org/10.1016/j.soilbio.2022.108675

#created 11 July 2022
#by:Kendalynn Morris and Mitchell Smith

#load ggplot2
library(ggplot2)
library(dplyr)
library(lubridate)

#read in collar ids and data
collars <- read.csv("cores_collars.csv")
data_raw <- read.csv("UPDATED COLLAR HEIGHT AVG.csv")

#update timestamp for new raw_data
data_raw$timestamp <- mdy_hm(data_raw$datetime, tz="EST")
unique(date(data_raw$timestamp))

data_raw$date <- date(data_raw$timestamp)

#fix a data entry error where collar 69 was entered as 59
#this must be run before 693 is reassigned to 69
data_raw[data_raw$Collar == "69",]$Collar <- "59"

#filter bad values and replace none standard collar names
data_raw %>%
  filter(FCH4_dry != -9999,
         FCO2_dry > 0) %>%
  mutate(Collar = replace(Collar,
                          Collar == "3-Feb", "3"),
         Collar = replace(Collar,
                          Collar == "2-Feb", "2"),
         Collar = replace(Collar,
                          Collar == "639", "63"),
         Collar = replace(Collar,
                          Collar == "693", "69")) -> data_raw

collars$Collar <- as.character(collars$Collar)

#join experiment ids and data
data <- left_join(data_raw, collars,
            by = "Collar",
            keep = FALSE)

#Fill in NA's in reps with 1
data[is.na(data$Reps),]$Reps <- 1

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

#summer 2022 version of Hopple graph
data %>% filter(Location == "g_up", Origin == "g_up") -> upland
data %>% filter(Location == "g_low", Origin %in% c("g_up", "g_low")) -> lowland

upland$Movement <- "upland-upland"

lowland$Movement <- "upland-lowland"
lowland[lowland$Origin == "g_low",]$Movement <- "lowland-lowland"

upland %>% bind_rows(lowland) -> uplow
ggplot(data = uplow, aes(Movement, FCO2_dry,)) +
  geom_boxplot()

####14-07-2022####
#KM example plot

ggplot(data = data[data$FCH4_dry < 1500,],
       aes(date(timestamp), FCH4_dry,
           fill = Experiment, group = date(timestamp))) +
  geom_boxplot() +
  facet_grid(Origin~., scales = "free")

#alternate example
data %>%
  dplyr::filter( FCH4_dry < 1500,
                 Origin %in% c('g_low','g_mid','g_up') ) %>%
  ggplot() +
  aes( x = timestamp, y = FCH4_dry, fill = Experiment ) +
  geom_boxplot() +
  facet_grid(Origin~., scales = 'free')

#Filter g_low and Control
lowcontrol <- filter(data, Origin == "g_low" &
                         Experiment == "Control")






#Soil Moisture Statistics
SWC_stats <- data %>% group_by(Origin, Location) %>%
    summarize(sdev = sd(SWC_mean),
              minimun = min(SWC_mean),
              maximum = max(SWC_mean),
              mean = mean(SWC_mean),
              med = median(SWC_mean)) %>%
    mutate(variable = "SWC_mean")

FCH4_stats <- data %>% group_by(Origin, Location) %>%
    summarize(sdev = sd(FCH4_dry),
              minimun = min(FCH4_dry),
              maximum = max(FCH4_dry),
              mean = mean(FCH4_dry),
              med = median(FCH4_dry)) %>%
    mutate(variable = "FCH4_dry")

FCO2_stats <- data %>% group_by(Origin, Location) %>%
    summarize(sdev = sd(FCO2_dry),
              minimun = min(FCO2_dry),
              maximum = max(FCO2_dry),
              mean = mean(FCO2_dry),
              med = median(FCO2_dry)) %>%
    mutate(variable = "FCO2_dry")

#Temperature Statistics
TS_stats <- data %>% group_by(Origin, Location) %>%
    summarize(sdev = sd(TS_mean),
              minimun = min(TS_mean),
              maximum = max(TS_mean),
              mean = mean(TS_mean),
              med = median(TS_mean)) %>%
    mutate(variable = "TS_mean")

FCH4_stats <- data %>% group_by(Origin, Location) %>%
    summarize(sdev = sd(FCH4_dry),
              minimun = min(FCH4_dry),
              maximum = max(FCH4_dry),
              mean = mean(FCH4_dry),
              med = median(FCH4_dry)) %>%
    mutate(variable = "FCH4_dry")

stats <- bind_rows(TS_stats,FCH4_stats)



#Temp & CO2
ggplot(data = data[data$FCO2_dry<25,], aes(TS_mean, FCO2_dry)) +
  geom_point()

#Temp & CH4
ggplot(data = data[data$FCH4_dry<200,], aes(TS_mean, FCH4_dry)) +
  geom_point()

#SWC & CO2
ggplot(data = data[data$FCO2_dry<25,], aes(SWC_mean,FCO2_dry)) +
  geom_point()

#SWC & CH4
ggplot(data = data[data$FCH4_dry<200,], aes(SWC_mean,FCH4_dry)) +
  geom_point()



# Good graph
ggplot(data = data, aes(date(timestamp),SWC_mean)) +
  geom_boxplot(aes(group = date(timestamp), fill = Origin))

# timestamp example
ggplot(data, aes(date(timestamp), FCH4_dry, color = SWC_mean)) +
  geom_point() +
  facet_wrap(~Origin, scales = "free")



#experimental graphing (finalists)
# 1
data %>%
  dplyr::filter( FCH4_dry < 5 & FCH4_dry >-5,
                 Origin %in% c('g_low','g_mid','g_up')) %>%
  ggplot() +
  aes( x = timestamp, y = FCH4_dry, fill = Experiment ) +
  geom_point() +
  facet_grid(Location~Experiment, scales = 'free') +
  theme(axis.text.x = element_text(angle = 90))



#Temp on CH4 - Comparing Origins
ggplot(data = data[data$FCH4_dry<200,], aes(TS_mean,FCH4_dry, color = Origin)) +
    geom_point() +
    facet_wrap(~Origin, scales = "free")

#SWC & CH4 - Comparing Origins
ggplot(data = data[data$FCH4_dry<200,], aes(SWC_mean, FCH4_dry, color = Origin)) +
  geom_point() +
  facet_wrap(~Location, scales = "free") +
  labs(title = "Effect of SWC on CH4")





#Graphs for powerpoint presentation

# Compare CH4 and SWC_mean between plots
data %>%
  dplyr::filter( FCH4_dry < 400 & FCH4_dry >-5,
                 Origin %in% c('g_low','g_mid','g_up')) %>%
ggplot(aes(date(timestamp), FCH4_dry, color = SWC_mean)) +
    scale_color_distiller(direction = 1) +
  geom_point() +
    labs(x = "Date", y = "Flux CH4", color = "Soil Moisture") +
  facet_wrap(~Origin) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90))
ggsave("CH4 flux by date (SWC).png", width = 6, height = 4)

# Compare CO2 and SWC_mean between plots
data %>%
  dplyr::filter( FCO2_dry < 25 & FCO2_dry >-5,
                 Origin %in% c('g_low','g_mid','g_up')) %>%
  ggplot(aes(date(timestamp), FCO2_dry, color = SWC_mean)) +
    scale_color_distiller(direction = 1) +
  geom_point() +
    labs(x = "Date", y = "Flux CO2", color = "Soil Moisture") +
  facet_wrap(~Origin) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90))
ggsave("CO2 flux by date (SWC).png", width = 6, height = 4)

# Compare CH4 and TS_mean between plots
data %>%
    dplyr::filter( FCH4_dry < 400 & FCH4_dry >-5,
                   Origin %in% c('g_low','g_mid','g_up')) %>%
    ggplot(aes(date(timestamp), FCH4_dry, color = TS_mean)) +
    scale_color_distiller(palette = "YlOrBr", direction = 1) +
    #scale_color_gradient2( low = 'yellow', mid = orange, high = 'red') +
    geom_point() +
    labs(x = "Date", y = "Flux CH4", color = "Temp (C)") +
    facet_wrap(~Origin) +
    theme_dark() +
    theme(axis.text.x = element_text(angle = 90))
ggsave("CH4 flux by date (Temp).png", width = 6, height = 4)

# Compare CO2 and TS_mean between plots
data %>%
  dplyr::filter( FCO2_dry < 25 & FCO2_dry >-5,
                 Origin %in% c('g_low','g_mid','g_up')) %>%
  ggplot(aes(date(timestamp), FCO2_dry, color = TS_mean)) +
  scale_color_distiller(palette = "YlOrBr", direction = 1) +
  geom_point() +
    labs(x = "Date", y = "Flux CO2", color = "Temp (C)") +
  facet_wrap(~Origin) +
  theme_dark() +
    theme(axis.text.x = element_text(angle = 90))
ggsave("CO2 flux by date (Temp).png", width = 6, height = 4)

# Hopple Model Graph A (CO2)
upland %>% bind_rows(lowland) -> uplow
ggplot(data = uplow, aes(week(timestamp), FCO2_dry)) +
    geom_boxplot(alpha = 0.2, width = 0.4, aes(color = Movement, group = interaction(Movement, week(timestamp)), fill = Movement)) +
    geom_smooth(aes(color = Movement), se = FALSE) +
    labs(x = "Calendar Week", y = "Flux CO2") +
    scale_fill_manual(values = c("lowland-lowland" = "#f73bac",
                                 "upland-lowland" = "#cb81e6", "upland-upland" = "#19cf9b")) +
    scale_color_manual(values = c("lowland-lowland" = "#f73bac",
                                "upland-lowland" = "#9b42f5", "upland-upland" = "#2eab5e")) +
    theme_bw()
ggsave("CO2 flux Hopple Model.png", width = 12.5, height = 4)

# Hopple Model Graph B (CO2)
upland %>% bind_rows(lowland) -> uplow
ggplot(data = uplow, aes(Movement, FCO2_dry, fill = Movement)) +
    geom_boxplot(alpha = 0.5, aes(color = Movement)) +
    labs(x = "Movement", y = "Flux CO2") +
    scale_fill_manual(values = c("lowland-lowland" = "#f73bac",
                                 "upland-lowland" = "#cb81e6", "upland-upland" = "#19cf9b")) +
    scale_color_manual(values = c("lowland-lowland" = "#f73bac",
                                "upland-lowland" = "#9b42f5", "upland-upland" = "#2eab5e")) +
    theme_bw()



#Graphs for Research Report

#CO2
data %>%
    dplyr::filter(Origin %in% c('g_low','g_mid','g_up'),
                  week(timestamp) == 19) %>%
    ggplot(aes(week(timestamp), FCO2_dry, fill = Location)) +
    geom_boxplot(aes(group = interaction(week(timestamp),
                                         Location))) +
    facet_grid(.~Origin) +
    labs(title = "Interaction of Origin & Location") +
    theme_bw()

#CH4
data %>%
    dplyr::filter(FCH4_dry < 10 & FCH4_dry >-5,
                Origin %in% c('g_low','g_mid','g_up'),
                week(timestamp) == 26) %>%
    ggplot(aes(week(timestamp), FCH4_dry, fill = Location)) +
    geom_boxplot(aes(group = interaction(week(timestamp),
                                         Location))) +
    facet_grid(.~Origin) +
    labs(title = "Interaction of Origin & Location") +
    theme_bw()


data %>%
    dplyr::filter( FCH4_dry < 5 & FCH4_dry >-5,
                   Origin %in% c('g_low','g_mid','g_up')) %>%
    ggplot() +
    aes( x = timestamp, y = FCH4_dry, fill = Experiment ) +
    geom_point() +
    facet_grid(Location~Experiment, scales = 'free') +
    theme(axis.text.x = element_text(angle = 90))


# Good graph
ggplot(data = data, aes(date(timestamp),SWC_mean)) +
    geom_boxplot(aes(group = date(timestamp), fill = Origin))

# timestamp example
ggplot(data, aes(date(timestamp), FCH4_dry, color = SWC_mean)) +
    geom_point() +
    facet_wrap(~Origin, scales = "free")

#What is the effect of SWC on CO2
ggplot(data = data[data$FCO2_dry<50,], aes(SWC_mean, FCO2_dry, color = Origin)) +
  geom_point() +
  facet_wrap(~Location, scales = "free") +
  labs(title = "Effect of SWC on CO2")

#What is the effect of temp on CH4
ggplot(data = data[data$FCH4_dry<200,], aes(TS_mean,FCH4_dry, color = Origin)) +
  geom_point() +
  facet_wrap(~Origin, scales = "free") +
  labs(title = "Effect of Temperature on CH4")

#What is the effect of temp on CO2
ggplot(data = data[data$FCO2_dry<50,], aes(TS_mean,FCO2_dry, color = Origin)) +
  geom_point() +
  facet_wrap(~Origin, scales = "free") +
  labs(title = "Effect of Temperature on CO2")


#Facet Location - compare SWC with fluxes
data %>%
  ggplot() +
  geom_point(aes(x = SWC_mean, y = FCO2_dry)) +
  facet_wrap(~Location) +
  labs(title = "Soil Moisture Effect On CO2")

data %>%
  ggplot() +
  geom_point(aes(x = SWC_mean, y = FCH4_dry, colour = "blue")) +
  facet_wrap(~Location, scales = "free") +
  labs(title = "Soil Moisture Effect On CH4")

#Facet individual plot & collar location
data %>%
  dplyr::filter(Location %in% c("g_low")) %>%
  ggplot() +
  geom_point(aes(x = SWC_mean, y = FCH4_dry)) +
  facet_wrap(~Location + Collar, scales = "free") +
  labs(title = "Soil Moisture Effect On CH4")

