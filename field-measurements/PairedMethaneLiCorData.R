
#This code is to read-in and clean-up
#2022 soil methane and respiration data
#from the HS (GCREW) transect of the transplant experiment
#see Hopple et al 2022 for more detail:
#https://doi.org/10.1016/j.soilbio.2022.108675

#created 11 July 2022
#by:Kendalynn Morris and Mitchell Smith

library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

#read in collar ids and data
collars <- read.csv("cores_collars.csv")
data_raw <- read.csv("licordata.csv")

#NOTE!
#difference between export.csv (from Soil Flux Pro)
#& licordata.csv, licordata.csv has extra rows removed
#and headings changed to make R happy
#export.csv has R2 of linear fits

#format timestamp
data_raw$timestamp <- mdy_hm(data_raw$datetime, tz="EST")
#check dates
#should have May 11th to August 17th
sort(unique(date(data_raw$timestamp)))
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

#summarize within sampling sessions

#step one remove 2nd measurements from days of soil sampling

#7/27 and 7/29
data %>%
    filter(as.character(date)
           %in% c("2022-07-27",
                  "2022-07-29"),
           Reps == 1,
           Location != "g_mid") -> collection
#cut out first measurement for collar 30 on the 27th
data %>%
    filter(Reps == 2, Collar == 30,
           as.character(date) == "2022-07-29") %>%
    bind_rows(collection) -> add

#summarize by date and rep
data %>%
    filter(! as.character(date)
           %in% c("2022-07-27",
                  "2022-07-29")) %>%
    bind_rows(add) %>%
    group_by(date, Collar, Reps, Origin,
             Location, Experiment) %>%
    summarise(timestamp = mean(timestamp),
              FCH4 = median(FCH4_dry),
              FCO2 = median(FCO2_dry),
              SWC = mean(SWC_mean),
              TA = mean(TA_mean),
              TS = mean(TS_mean)) %>%
    filter(if_else(Location == "g_low",
                   FCH4 < 2000, FCH4 < 300)) %>% #trim outliers
    relocate(date, .after = Reps)-> f_dat

#export for later use
#RTA = Ready To Analyze
#write.csv(f_dat, "licorRTA.csv")

#for the curious
#Collar Height
height_stats <- data %>% group_by(Collar) %>%
    summarize(sdev = sd(Height),
              minimun = min(Height),
              maximum = max(Height),
              mean = mean(Height),
              med = median(Height)) %>%
    mutate(variable = "Height")
