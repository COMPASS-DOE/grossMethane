
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
library(tidyr)
library(lubridate)

#read in collar ids and data
collars <- read.csv("cores_collars.csv")
data_raw <- read.csv("licordata.csv")

#NOTE!
#difference between export.csv and licordata.csv
#extra rows removed and headings changed to make R happy
#export.csv has R2 of linear fits

#format timestamp
data_raw$timestamp <- mdy_hm(data_raw$datetime, tz="EST")
#check dates
unique(date(data_raw$timestamp))
#create seperate date column
data_raw$date <- date(data_raw$timestamp)

#fix a data entry error where collar 69 was entered as 59
#this must be run before 693 is reassigned to 69
data_raw[data_raw$Collar == "69",]$Collar <- "59"

#filter bad values and replace none standard collar names
data_raw %>%
  filter(FCH4_dry != -9999,
         FCO2_dry > 0) %>%
  mutate(Collar = replace(Collar,
                          Collar == "639", "63"),
         Collar = replace(Collar,
                          Collar == "693", "69")) -> data_raw

#format Collar
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

#Labels for graphing later
origin.labs <- c("lowland origin", "midland origin", "upland origin")
names(origin.labs) <- c("g_low", "g_mid", "g_up")

#export for later use
#write.csv(data, "licorRTA.csv")


#summarise by date, take average values
# data %>%
#     group_by(date, Collar) %>%
#     summarise(n = n()) -> CollarDate



#summer 2022 version of Hopple graph
data %>% filter(Location == "g_up",
                Origin == "g_up") -> upland
data %>% filter(Location == "g_low",
                Origin %in% c("g_up", "g_low")) -> lowland

upland$Movement <- "upland-upland"
lowland$Movement <- "upland-lowland"

lowland[lowland$Origin == "g_low",]$Movement <- "lowland-lowland"

upland %>% bind_rows(lowland) -> uplow

# Hopple Model Graph A (CO2)
ggplot(data = uplow, aes(week(timestamp), FCO2_dry)) +
    geom_boxplot(alpha = 0.2, width = 0.4, aes(color = Movement, group = interaction(Movement, week(timestamp)), fill = Movement)) +
    geom_smooth(aes(color = Movement), se = FALSE) +
    labs(x = "Calendar Week", y = "Flux CO2") +
    scale_fill_manual(values = c("lowland-lowland" = "#f73bac",
                                 "upland-lowland" = "#cb81e6", "upland-upland" = "#19cf9b")) +
    scale_color_manual(values = c("lowland-lowland" = "#f73bac",
                                  "upland-lowland" = "#9b42f5", "upland-upland" = "#2eab5e")) +
    theme_bw()
#ggsave("CO2 flux Hopple Model.png", width = 12.5, height = 4)

# Hopple Model Graph B (CO2)
ggplot(data = uplow, aes(Movement, FCO2_dry, fill = Movement)) +
    geom_boxplot(alpha = 0.5, aes(color = Movement)) +
    labs(x = "Movement", y = "Flux CO2") +
    scale_fill_manual(values = c("lowland-lowland" = "#f73bac",
                                 "upland-lowland" = "#cb81e6", "upland-upland" = "#19cf9b")) +
    scale_color_manual(values = c("lowland-lowland" = "#f73bac",
                                  "upland-lowland" = "#9b42f5", "upland-upland" = "#2eab5e")) +
    theme_bw()

#Most of the action is in the lowland plot

hist(data$FCH4_dry)
hist(log10(data$FCH4_dry + 20))

data$log <- log10(data$FCH4_dry + 20)
#Filter g_low and Control
low <- filter(data, Location == "g_low")






#Summaries
TS_stats <- data %>% group_by(Origin, Location) %>%
    summarize(sdev = sd(TS_mean),
              minimun = min(TS_mean),
              maximum = max(TS_mean),
              mean = mean(TS_mean),
              med = median(TS_mean)) %>%
    mutate(variable = "TS_mean")

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

height_stats <- data %>% group_by(Collar) %>%
    summarize(sdev = sd(Height),
              minimun = min(Height),
              maximum = max(Height),
              mean = mean(Height),
              med = median(Height)) %>%
    mutate(variable = "Height")


#example graphs of Mitchell's
data %>%
    dplyr::filter(Origin %in% c('g_low','g_mid','g_up'),
                  week(timestamp) == 19) %>%
    ggplot(aes(week(timestamp), FCO2_dry, fill = Location)) +
    geom_boxplot(aes(group = interaction(week(timestamp), Location))) +
    facet_grid(.~Origin, labeller = labeller(Origin = origin.labs)) +
    labs(x = "Calendar Week", y = "Flux CO2", title = "Interaction of Origin & Location on CO2") +
    theme_bw()

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

