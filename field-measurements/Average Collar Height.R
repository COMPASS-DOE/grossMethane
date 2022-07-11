#Mitchell, July 5 2022 
#Average of 4 Collar Height Measurements 
#For Paired Methane Project
library(tidyr)
library(dplyr)
Data <- read.csv("GitHub/grossMethane/field-measurements/Collar Height Measurements.csv")
str(Data)

Data %>%
  rowwise() %>%
mutate(average=mean(c(Rep1,Rep2,Rep3,Rep4))) -> Data 

Data[Data$collar == 17,]$average
