library(tibble)
library(ggplot2)
library(ggpmisc)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
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
    group_by(Collar) %>%
    mutate(P = mean(P, na.rm = TRUE),
              k = mean(k, na.rm = TRUE),
              k0 = mean(k0, na.rm = TRUE),
              ml_k = mean(ml_k, na.rm = TRUE),
              net_sd = sd(net, na.rm = TRUE),
              net = mean(net, na.rm = TRUE),
              umolPg = mean(umolPg, na.rm = TRUE),
              umolkg = mean(umolkg, na.rm = TRUE),
              n = n()) %>%
    ungroup() -> graph_sums

#summary of net rates
ggplot(graph_sums,
       aes(Location, net, fill = Origin)) +
    scale_x_discrete(labels = c("lowland",
                                "upslope")) +
    scale_fill_discrete(labels = Olabs) +
    geom_boxplot()

#summary of production
ggplot(graph_sums,
       aes(Location, umolPg, fill = Origin)) +
    scale_x_discrete(labels = c("lowland",
                                "upslope")) +
    scale_fill_discrete(labels = Olabs) +
    geom_boxplot()

ggplot(graph_sums,
       aes(sm, umolPg, color = Origin,
           shape = Location)) +
    geom_point(size = 3)

ggplot(graph_sums,
       aes(sm, umolkg, color = Origin,
           shape = Location)) +
    geom_point(size = 3)

ggplot(graph_sums,
       aes(umolPg, umolkg, color = Origin,
           shape = Location)) +
    geom_point(size = 3)
