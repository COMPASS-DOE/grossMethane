
library(emojifont)

#summary of terminal delta values
ggplot(data[data$round != "T5",],
       aes(round, `HR Delta iCH4 Mean`,  color = as.factor(order))) +
  #scale_color_discrete(labels = Olabs) +
  geom_point(aes(fill = Location, size = `HR Delta iCH4 Std`)) +
  facet_grid(Location~.) +
  geom_smooth(aes(group = interaction(Origin, Location)),
              se = FALSE) + theme_bw()


#gross production vs soil moisutre
ggplot(data = data[data$round == "T2",], aes(sm, umolPg)) +
  geom_point(aes(color = Origin), size = 3) +
  scale_color_discrete(labels = Olabs) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..rr.label..,
                                 sep = "~~~")))+
  labs(x= "soil water content
       (H\u2082O * g\uFE63\u00b9 dry soil)",
       y= "Gross Production
       (\u00b5mol CH\u2084 * g\uFE63\u00b9 dry soil * d\uFE63\u00b9)")

#\u207b

#gross consumption vs soil moisture
ggplot(data = data[data$round == "T1",], aes(sm, umolCg)) +
    geom_point(aes(color = Origin), size = 3) +
    scale_color_discrete(labels = Olabs) +
    geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
    stat_poly_eq(formula = y ~ x,
                 aes(label = paste(..rr.label..,
                                   sep = "~~~")))+
    labs(x= "soil water content
       (H\u2082O * g\uFE63\u00b9 dry soil)",
         y= "Gross Consumption
       (-\u00b5mol CH\u2084 * g\uFE63\u00b9 dry soil * d\uFE63\u00b9)")


#gross consumption vs concentration
ggplot(data = na.omit(data), aes(round, umolCg)) +
    geom_point(aes(color = Origin, size = (Mppm + M13ppm))) +
    scale_color_discrete(labels = Olabs) +
    geom_smooth(aes(color = Origin, group = Collar),
                method = "loess", se = FALSE) +
    labs(x= "Time",
         y= "Gross Consumption
       (-\u00b5mol CH\u2084 * g\uFE63\u00b9 dry soil * d\uFE63\u00b9)")

#gross consumption vs concentration
ggplot(data = na.omit(data[data$Collar != 52 &
                       data$ap_cor > 0,]), aes((Mppm + M13ppm), umolCg)) +
    geom_point(aes(color = Location, size = (umolPg), shape = Location)) +
    scale_shape_discrete(labels = c("lowland", "upland")) +
    scale_color_discrete(labels = c("lowland", "upland")) +
    scale_size_continuous("Gross Production") +
    labs(x= "Methane Concentration (ppm)",
         y= "Gross Consumption
       (\u00b5mol CH\u2084 * g\uFE63\u00b9 dry soil * d\uFE63\u00b9)") +
    theme(text = element_text(size = 18))


ggplot(data = na.omit(data[data$Collar != 52 &
                           data$time_days < 0.2 &
                               data$ap_cor > 0,]), aes(time_days, (umolPg - umolCg))) +
    geom_jitter(aes(color = Location, size = (Mppm + M13ppm), shape = Location)) +
    scale_shape_discrete(labels = c("lowland", "upland")) +
    scale_color_discrete(labels = c("lowland", "upland")) +
    scale_size_continuous("Total Methane (ppm)") +
    labs(x= "Time (d)",
         y= "Net Rate
       (\u00b5mol CH\u2084 * g\uFE63\u00b9 dry soil * d\uFE63\u00b9)")

#gross consumption vs gross production
ggplot(data = na.omit(data), aes(umolPg, umolCg), shape = Location) +
    geom_jitter(aes(color = sm), size = 5) +
    scale_shape_discrete(labels = c("lowland", "upland")) +
    scale_color_distiller("SWC") +
    #scale_size_continuous("Methane Concentration (ppm)") +
    geom_abline(slope = 1) + lims(x = c(0,0.15), y = c(0,0.15)) +
    geom_smooth(method = lm, formula = y ~ x, se = FALSE, size = 0.75) +
    #stat_poly_eq(formula = y ~ x)+
                 # aes(label = paste(..eq.label..,
                 #                   sep = "~~~",
                 #                   size = 48)))+
    #facet_wrap(round~.) +
    labs(x=  "Gross Production
       (\u00b5mol CH\u2084 * g\uFE63\u00b9 dry soil * d\uFE63\u00b9)",
         y= "Gross Consumption
       (\u00b5mol CH\u2084 * g\uFE63\u00b9 dry soil * d\uFE63\u00b9)") +
    theme(text = element_text(size = 36), legend.position = "right")

size_range <- range(data$umolPg + 0.0075)/max(data$umolPg + 0.0075) * 6

#gross consumption vs soil moisture
ggplot(data = na.omit(data), aes(sm, umolCg)) +
    geom_jitter(aes(color = Location, size = (umolPg), shape = Location)) +
    scale_shape_discrete(labels = c("lowland", "upland")) +
    scale_color_discrete(labels = c("lowland", "upland")) +
    scale_size_continuous("Gross Production",
                          range = size_range) +
    labs(x= "soil water content
       (H\u2082O * g\uFE63\u00b9 dry soil)",
         y= "Gross Consumption
       (\u00b5mol CH\u2084 * g\uFE63\u00b9 dry soil * d\uFE63\u00b9)") +
    guides(shape = guide_legend(override.aes = list(size = 5))) +
    theme(text = element_text(size = 36))
