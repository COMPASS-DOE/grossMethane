# This code is for analyzing and graphing
# gross methane fluxes from GCREW soils.
# 13C tracer incubations were conducted the last week of July 22
# in wetland edge (aka lowland aka transition) and upland soils

# September 2022
# Kendalynn A. Morris

#read results generated on September 13th
#from commit "venture into toyland"

pk_results <- read.csv("13092022_pk_results.csv")


#read in soil moisture data
soil <- read.csv("field-measurements/July22_soilmoisture.csv")
data <- merge(soil, pk_results, by = "id")
#calculate soil dry mass and fresh water mass
data$mass <- data$jdry - data$jempty
data$sm <- data$jfresh - data$jdry
#calculate umol of gas per g dry soil per day for Production
data$umolPg <- (data$P * 44.64)/data$mass
#calculate umol of gas per g dry soil per day for Consumption
data$umolKg <- (data$k * 44.64)/data$mass

#add upland vs lowland, does not currently work
upland <- c(60, 44, 85, 56, 86, 41, 54, 43, 58, 53, 87, 59, 55, 42)
data %>% mutate(plot = if_else(id %in% upland, "upland",
                               "lowland")) -> data

ggplot(data = data, aes(umolPg, umolKg, colour = sm)) +
  geom_point(size = 3) +
  geom_smooth(method = lm, formula = y ~ x) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")))

ggplot(data = data, aes(sm, umolPg, colour = sm)) +
  geom_point(size = 3) +
  facet_grid(plot ~ .) +
  geom_smooth(method = lm, formula = y ~ x) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")))

ggplot(data = data, aes(sm, umolKg, colour = sm)) +
  geom_point(size = 3) +
  facet_grid(plot ~ .) +
  geom_smooth(method = lm, formula = y ~ x) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")))
