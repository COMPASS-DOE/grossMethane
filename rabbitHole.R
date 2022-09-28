library(tibble)
library(ggplot2)
library(ggpmisc)
library(dplyr)
library(tidyr)
library(readr)
library(knitr)
library(lubridate)
library(car)
theme_set(theme_bw())

Olabs <- c("lowland", "midslope", "upslope", "midstream", "upstream")
Llabs <- c("lowland", "midslope", "upslope")
labellies <- c('g_low' = "lowland",
               'g_mid' = "midslope",
               'g_up' = "upslope",
               'midstream' = "midstream",
               'upstream' = "upstream")

ln_dat <- read.csv("27092022_ln_results.csv")
dat <- read.csv("27092022_results.csv")
paired <- read.csv("PairedData.csv")

ln_dat %>%
    filter(id != "17",
           net < 0) %>%
    mutate(Collar = as.character(id),
           k0fit = "ln") %>%
    select(-X, -id) %>%
    pivot_longer(cols = c("P", "k", "k0", "ml_k", "net"),
                 names_to = "variable") -> ln_dat2
dat %>%
    filter(id != "17",
           k < 0) %>%
    mutate(Collar = as.character(id),
           k0fit = "normal") %>%
    select(-X, -id) %>%
    pivot_longer(cols = c("P", "k", "k0", "ml_k", "net"),
                 names_to = "variable") %>%
    bind_rows(ln_dat2) %>%
        pivot_wider(id_cols = c(Collar, k0fit),
                    names_from = "variable") -> long

paired %>%
    select(Collar, Origin, Location,
           mass, sm, -X) %>%
    mutate(Collar = as.character(paired$Collar))  %>%
    right_join(long, by = "Collar") %>%
    mutate(umolPg = (P * 44.64)/mass, #calculate umol of gas per g dry soil per day for Production
           umolkg = (ml_k * 44.64/mass) #calculate umol of gas per g dry soil per day for Consumption
           ) -> graph

ggplot(graph,
       aes(Location, umolPg+umolKg, fill = Origin)) +
    scale_x_discrete(labels = c("lowland",
                                "upslope")) +
    scale_fill_discrete(labels = Olabs) +
    facet_grid(k0fit~.) +
    geom_boxplot()

graph %>%
    ungroup() %>%
    group_by(Collar, Location, Origin) %>%
    summarize(P = mean(P, na.rm = TRUE),
              k = mean(k, na.rm = TRUE),
              k0 = mean(k0, na.rm = TRUE),
              ml_k = mean(ml_k, na.rm = TRUE),
              net_sd = sd(net, na.rm = TRUE),
              net = mean(net, na.rm = TRUE),
              sm = mean(sm, na.rm = TRUE),
              umolPg = mean(umolPg, na.rm = TRUE),
              umolkg = mean(umolkg, na.rm = TRUE),
              n = n()) %>%
    ungroup() -> graph_sums

#summary of lab net rates
ggplot(graph_sums,
       aes(Location, net, fill = Origin)) +
    scale_x_discrete(labels = c("lowland",
                                "upslope")) +
    scale_fill_discrete(labels = Olabs) +
    labs(x= "Location",
         y= "Net Rate \n (\u00b5mol CH\u2084 * g\u207b\u00b9 dry soil * d\u207b\u00b9)") +
    theme_bw() +
    geom_boxplot()

#summary of field net rates
ggplot(paired[paired$FCH4 < 360,],
       aes(Origin, FCH4, fill = Origin)) +
    scale_x_discrete(labels = Olabs,
                     ) +
    scale_fill_discrete(labels = Olabs) +
    labs(x= " ",
         y= "Net Rate \n (nmol CH\u2084 * m\u207b\u00B2 * s\u207b\u00b9)") +
    theme_bw() + geom_boxplot() +
    facet_grid(Location ~ . , scales = 'free',
               labeller = as_labeller(labellies)) +
    theme(axis.text.x = element_text(angle = 22.5,
            hjust = 1))

#summary of production
ggplot(graph_sums,
       aes(Location, umolPg, fill = Origin)) +
    scale_x_discrete(labels = c("lowland",
                                "upslope")) +
    scale_fill_discrete(labels = Olabs) +
    labs(x= "Location",
         y= "production \n (\u00b5mol CH\u2084 * g\u207b\u00b9 dry soil * d\u207b\u00b9)") +
    geom_boxplot()


ggplot(graph_sums, aes(sm, umolPg)) +
    geom_point(aes(color = Origin, shape = Location),
               size = 3) +
    geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
    stat_poly_eq(formula = y ~ x,
                 aes(label = paste(..eq.label..,
                                   ..rr.label..,
                                   ..p.value.label..,
                                   sep = "~~~"))) +
    scale_color_discrete(labels = Olabs) +
    scale_shape_discrete(labels = c("lowland",
                                    "upland")) +
    labs(x= "soil water content\n (H\u2082O * g\u207b\u00b9 dry soil)",
         y= "production \n (\u00b5mol CH\u2084 * g\u207b\u00b9 dry soil * d\u207b\u00b9)")
    theme_bw()

ggplot(graph_sums, aes(sm, umolkg)) +
    geom_point(aes(color = Origin, shape = Location),
               size = 3) +
    geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
    stat_poly_eq(formula = y ~ x, label.y = "bottom",
                 aes(label = paste(..eq.label..,
                                   ..rr.label..,
                                   ..p.value.label..,
                                   sep = "~~~"))) +
    scale_color_discrete(labels = Olabs) +
    scale_shape_discrete(labels = c("lowland",
                                    "upland")) +
    labs(x= "soil water content\n (H\u2082O * g\u207b\u00b9 dry soil)",
         y= "consumption \n (\u00b5mol CH\u2084 * g\u207b\u00b9 dry soil * d\u207b\u00b9)")
    theme_bw()

ggplot(graph_sums,
       aes(umolPg, umolkg, color = Origin,
           shape = Location)) +
    geom_point(size = 3)

hist(graph_sums$umolPg)
hist(log10(graph_sums$umolPg+50))
graph_sums$logP <- log10(graph_sums$umolPg + 50)
hist(graph_sums$umolkg)
hist(sqrt(graph_sums$umolkg + 1050))
graph_sums$rtK <- sqrt(graph_sums$umolkg + 1050)


consumption <- aov(rtK ~ Location,
                   data = graph_sums)
summary(consumption, type="III")
par(mfrow=c(2,2))
plot(consumption)
dev.off()

production <- aov(logP ~ Location + sm,
                  data = graph_sums)
summary(production, type="III")
par(mfrow=c(2,2))
plot(production)
dev.off()

field <- aov(FCH4 ~ Location,
                  data = paired)
summary(field , type="III")
par(mfrow=c(2,2))
plot(field)
dev.off()

soil_field <- aov(SWC ~ Location,
             data = paired)
summary(soil_field, type="III")
par(mfrow=c(2,2))
plot(soil_field)
dev.off()

soil_lab <- aov(sm ~ Origin,
            data = graph_sums)
summary(soil_lab , type="III")
par(mfrow=c(2,2))
plot(soil_lab)
dev.off()
