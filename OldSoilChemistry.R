
#This code is to read-in and summarize
#2020 soil chemistry data
#from PREMIS
#see Hopple et al 2022 for more detail:
#https://doi.org/10.1016/j.soilbio.2022.108675

#created 25 May 2023
#by:Kendalynn Morris

soil_chem <- read.csv("transplant_soil_chemistry_042020_short.csv")
collars <- read.csv("2022collars.csv")

library(dplyr)

data2022 <- collars %>%
  mutate(Collar = as.integer(Collar)) %>%
  left_join(soil_chem, by = "Collar") %>%
  filter(Depth == 5)

data2022 %>%
  group_by(Location, Origin) %>%
  select(-Collar, -Depth, -meanCO2, -meanT5) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) -> means

means %>%
  ungroup() %>%
  mutate(across(where(is.numeric)), signif(., digits = 3), na.rm = TRUE) -> round_means

data2022 %>%
  group_by(Location, Origin) %>%
  select(-Collar, -Depth, -meanCO2, -meanT5) %>%
  summarise_if(is.numeric, sd, na.rm = TRUE) -> errors

library(qwraps2)
summary_table(mtcars2, summaries = our_summary1, by = c("cyl_factor"))
summary_table(data2022, dplyr::group_by(Location, Origin))

data2022 %>%
  group_by(Location, Origin) %>%
  select(-Collar, -Depth, -meanCO2, -meanT5, -meanSM) %>%
  summary()
