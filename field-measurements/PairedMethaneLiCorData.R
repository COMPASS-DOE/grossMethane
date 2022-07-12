
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

#first summary plot of CO2 fluxes
ggplot(data = data, aes(Experiment, FCO2_dry)) +
  geom_boxplot()



