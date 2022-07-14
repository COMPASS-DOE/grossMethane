
#This code is to read-in, graph, and analyze
#2022 soil methane and respiration data
#from the HS (GCREW) transect of the transplant experiment
#see Hopple et al 2022 for more detail:
#https://doi.org/10.1016/j.soilbio.2022.108675

#created 11 July 2022
#by:Kendalynn Morris and Mitchell Smith

#load packages
library(ggplot2)
library(dplyr)
library(lubridate)

#read in collar ids and data
collars <- read.csv("cores_collars.csv")
data_raw <- read.csv("licordata.csv")

data_raw[data_raw$Collar == "69",]$Collar <- "59"

data_raw %>%
  filter(FCH4_dry != -9999,
         FCO2_dry > 0) %>%
  mutate(Collar = replace(Collar,
                          Collar == "3-Feb", "3"),
         Collar = replace(Collar,
                          Collar == "2-Feb", "2")) -> data_raw

collars$Collar <- as.character(collars$Collar)

#join experiment ids and data
data <- left_join(data_raw, collars,
            by = "Collar",
            keep = FALSE)

#remove NA's caused by 630 and 693 for now
data <- data[!is.na(data$Site),]

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

data$Location <- recode_factor(data$Location,
                             "HSLE" = "g_low",
                             "HSME" = "g_mid",
                             "HSHE" = "g_up",
                             "MSLE" = "midstream",
                             "LSLE" = "upstream")


#create timestamp
data$timestamp <- paste(data$date, data$time)
data$timestamp <- mdy_hms(data$timestamp, tz="EST")



#639 vs 693, who is who?

#isolate targets
sixes <- data %>%
  filter(as.numeric(Collar) > 600)
sixes$group <- ""
sixes[sixes$Collar == "639",]$group <- "target639"
sixes[sixes$Collar == "693",]$group <- "target693"

#isolate comparison collars from g_low
others <- data %>%
  filter(Location == "g_low",
         Collar %in% c("1", "2", "3", "4",
                     "69", "70", "71", "72",
                     "61", "62", "63", "64")) %>%
  mutate(group = "control") %>%
  mutate(group = if_else(Collar %in% c("61","62","63","64"),"transplant","control"))

#combine
compare <- bind_rows(sixes, others)

#average data over date and group
compare %>%
  group_by(Collar, date, group) %>%
  summarise(across(everything(), mean)) -> compare

#CO2 fluxes
ggplot(compare[compare$date != "06/17/22",], 
       aes(group, FCO2_dry, fill = group)) + 
  geom_boxplot() +
facet_wrap(date~., scales = "free")

#CH4 fluxes
ggplot(compare[compare$date != "06/17/22",], 
       aes(group, FCH4_dry, fill = group)) + 
  geom_boxplot() +
  facet_wrap(date~., scales = "free")


#looks like 639 is likely a control collar
#and 693 is likely a transplant collar

