
#This code is to read-in, graph, and analyze
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
data_raw <- read.csv("licordata.csv")

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

#create timestamp
data$timestamp <- paste(data$date, data$time)
data$timestamp <- mdy_hms(data$timestamp, tz="EST")

#first summary plot of CO2 fluxes
ggplot(data = data, aes(Experiment, FCO2_dry,)) +
  geom_boxplot()

# FCO2_dry, TS_mean
ggplot(data = data, aes(FCO2_dry, TS_mean)) +
         geom_point(stat = "identity")

# TS_mean, FCO2_dry -> without outliers
ggplot(data = data[data$FCO2_dry<25,], aes(TS_mean, FCO2_dry)) +
  geom_point()

# date, FCO2_dry -> advanced
ggplot(data = data, aes(date,FCO2_dry)) +
  geom_boxplot(aes(group = date, fill = Origin))



#Date series plots -> CO2
ggplot(data = data, aes(date, FCO2_dry)) +
  geom_point()

#Date series plots -> CH4
ggplot(data = data, aes(date, FCH4_dry)) +
  geom_point()

#Date series plots -> TS_mean
ggplot(data = data, aes(date, TS_mean)) +
  geom_point()

#Date series plots -> SWC_mean
ggplot(data = data, aes(date, SWC_mean)) +
  geom_point()

#What is the effect of temp on CO2
# As Temp increases, CO2 also increases
ggplot(data = data[data$FCO2_dry<25,], aes(TS_mean, FCO2_dry)) +
  geom_point()

#What is the effect of temp on CH4 
# As Temp increases, CH4 remains the same
ggplot(data = data[data$FCH4_dry<200,], aes(TS_mean, FCH4_dry)) +
  geom_point()

#What is the effect of SWC on CO2
# As SWC increases, CO2 remains the same
ggplot(data = data[data$FCO2_dry<25,], aes(SWC_mean,FCO2_dry)) +
  geom_point()

#What is the effect of SWC on CH4
# As SWC increases, CH4 increases slightly  *filter out negative values for CH4
ggplot(data = data[data$FCH4_dry<200,], aes(SWC_mean,FCH4_dry)) +
  geom_point()

#Filter g_low and Control
lowcontrol <- filter(data, Origin == "g_low" & 
                            Experiment == "Control")


#Get boxes to be colored by origin
# helpful hint - use [] to filter out outliers

#200 for CH4
ggplot(data = data[data$FCH4_dry<200,], 
       aes(Experiment, FCH4_dry,)) +
  geom_boxplot()

# Good graph
ggplot(data = data, aes(date,TS_mean)) +
  geom_boxplot(aes(group = date, fill = Origin))

# timestamp test
ggplot(data, aes(hour(timestamp), FCO2_dry, color = TS_mean)) +
  geom_point() +
  facet_wrap(~Origin, scales = "free")
