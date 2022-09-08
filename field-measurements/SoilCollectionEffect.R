

# Did coring for lab incubations effect the flux?

library(ggplot2)
library(ggpmisc)
library(tidyr)
library(dplyr)

data <- read.csv("licorRTA.csv")

#remove data from days of soil sampling for testing
#7/27 and 7/29
data %>%
  filter(as.character(date) %in% c("2022-07-27",
                                   "2022-07-29"),
         Location != "g_mid") %>%
  select(-Site, -Installed, -timestamp,
         -Notes, -Core, -Core_placement,
         -date, -datetime, -Experiment,
         -Height) -> collection

collection[collection$Reps == 1,] ->rep1
#remove duplicate 85 for now (see note for BBL help below)
rep1 <- slice(rep1, -33)
#no rep 1 for 30
collection[collection$Reps == 2,] -> rep2
add <- slice(rep2, 9)
#add rep 2 for 30 as rep 1, remove from later subset
one <- bind_rows(rep1, add)
rep2 <- slice(rep2, -9)
collection[collection$Reps == 3,] -> rep3
two <- bind_rows(rep2, rep3)
two <- select(two, -Reps)

#create wide format dataset
one %>%
  select(-Reps) %>%
  full_join(two, by = c("Collar", "Origin", "Location"),
            suffix = c(".1", ".2")) %>%
  relocate(Origin, Location,
           .after= Collar) %>%
  na.omit() -> wideReps


#rename columns
#average values, timestamp?



ggplot(wideReps, aes(FCH4_dry.1, FCH4_dry.2)) +
  geom_point() + geom_abline(slope = 1, intercept = 0) +
    geom_smooth(method = lm) +
  ggtitle("Methane Pre and Post \n with outliers")

ggplot(wideReps[wideReps$FCH4_dry.2 < 3000 &
                  wideReps$FCH4_dry.1 < 1000,], aes(FCH4_dry.1, FCH4_dry.2)) +
  geom_point() + geom_abline(slope = 1, intercept = 0) +
  geom_smooth(method = lm) +
  ggtitle("Methane Pre and Post \n no outliers")

ggplot(wideReps, aes(FCO2_dry.1, FCO2_dry.2)) +
  geom_point() + geom_abline(slope = 1, intercept = 0) +
  geom_smooth(method = lm) +
  ggtitle("CO2 Pre and Post")



