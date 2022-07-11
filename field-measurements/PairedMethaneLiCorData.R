
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
  filter(FCH4_dry != -9999) %>%
  mutate(Collar = replace(Collar,Collar == "3-Feb", "3"),
         Collar = replace(Collar,Collar == "2-Feb", "2")) -> data_raw

collars$Collar <- as.character(collars$Collar)

#join experiment ids and data
data <- left_join(data_raw, collars,
            by = "Collar",
            keep = FALSE)
list(unique(data$Plot))

data %>%
rename("Origin" = "Plot") -> data

data$Origin <-
  recode_factor(data$Origin,
                "HSHE" = "g-up",
                "HSME" = "g-mid",
                "HSLE" = "g-low",
                "LSLE" = "upstream",
                "MSLE" = "midstream")

