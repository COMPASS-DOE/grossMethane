
#currently cannot run separately from main script

ggplot(f_dat, aes(week(timestamp), FCH4, fill = Origin)) +
    geom_boxplot() + facet_wrap(.~Location, scales = 'free')

f_dat %>%
    filter(Location == "g_low") -> g_low

ggplot(g_low[g_low$FCH4 < 200,], aes(week(timestamp), FCH4, fill = Origin)) +
    geom_boxplot(aes(group = interaction(week(timestamp), Origin)))

ggplot(g_low[g_low$Origin == "g_mid",], aes(week(timestamp), FCH4, color = Collar)) +
    geom_point(aes(group = interaction(week(timestamp), Collar)))

ggplot(f_dat, aes(week(timestamp), FCO2, fill = Origin)) +
    geom_boxplot() + facet_wrap(.~Location, scales = 'free')

#summer 2022 version of Hopple graph
f_dat %>% filter(Location == "g_up",
                Origin == "g_up") -> upland
f_dat %>% filter(Location == "g_low",
                Origin %in% c("g_up", "g_low")) -> lowland

upland$Movement <- "upland-upland"
lowland$Movement <- "upland-lowland"

lowland[lowland$Origin == "g_low",]$Movement <- "lowland-lowland"

upland %>% bind_rows(lowland) -> uplow

# Hopple Model Graph A (CO2)
ggplot(data = uplow, aes(week(timestamp), FCO2)) +
  geom_boxplot(alpha = 0.2, width = 0.4, aes(color = Movement, group = interaction(Movement, week(timestamp)), fill = Movement)) +
  geom_smooth(aes(color = Movement), se = FALSE) +
  labs(x = "Calendar Week", y = "Flux CO2") +
  scale_fill_manual(values = c("lowland-lowland" = "#f73bac",
                               "upland-lowland" = "#cb81e6", "upland-upland" = "#19cf9b")) +
  scale_color_manual(values = c("lowland-lowland" = "#f73bac",
                                "upland-lowland" = "#9b42f5", "upland-upland" = "#2eab5e")) +
  theme_bw()
#ggsave("CO2 flux Hopple Model.png", width = 12.5, height = 4)

ggplot(data = uplow, aes(week(timestamp), FCH4)) +
    geom_boxplot(alpha = 0.2, width = 0.4, aes(color = Movement, group = interaction(Movement, week(timestamp)), fill = Movement)) +
    geom_smooth(aes(color = Movement), se = FALSE) +
    labs(x = "Calendar Week", y = "Flux CH4") +
    scale_fill_manual(values = c("lowland-lowland" = "#f73bac",
                                 "upland-lowland" = "#cb81e6", "upland-upland" = "#19cf9b")) +
    scale_color_manual(values = c("lowland-lowland" = "#f73bac",
                                  "upland-lowland" = "#9b42f5", "upland-upland" = "#2eab5e")) +
    theme_bw()



# Hopple Model Graph B (CO2)
ggplot(data = uplow, aes(Movement, FCO2, fill = Movement)) +
  geom_boxplot(alpha = 0.5, aes(color = Movement)) +
  labs(x = "Movement", y = "Flux CO2") +
  scale_fill_manual(values = c("lowland-lowland" = "#f73bac",
                               "upland-lowland" = "#cb81e6", "upland-upland" = "#19cf9b")) +
  scale_color_manual(values = c("lowland-lowland" = "#f73bac",
                                "upland-lowland" = "#9b42f5", "upland-upland" = "#2eab5e")) +
  theme_bw()

ggplot(data = uplow, aes(Movement, FCH4, fill = Movement)) +
    geom_boxplot(alpha = 0.5, aes(color = Movement)) +
    labs(x = "Movement", y = "Flux CH4") +
    scale_fill_manual(values = c("lowland-lowland" = "#f73bac",
                                 "upland-lowland" = "#cb81e6", "upland-upland" = "#19cf9b")) +
    scale_color_manual(values = c("lowland-lowland" = "#f73bac",
                                  "upland-lowland" = "#9b42f5", "upland-upland" = "#2eab5e")) +
    theme_bw()

#Summaries
TS_stats <- f_dat %>% group_by(Origin, Location) %>%
  summarize(sdev = sd(TS),
            minimun = min(TS),
            maximum = max(TS),
            mean = mean(TS),
            med = median(TS)) %>%
  mutate(variable = "TS")

SWC_stats <- f_dat %>% group_by(Origin, Location) %>%
  summarize(sdev = sd(SWC),
            minimun = min(SWC),
            maximum = max(SWC),
            mean = mean(SWC),
            med = median(SWC)) %>%
  mutate(variable = "SWC")

FCH4_stats <- f_dat %>% group_by(Origin, Location) %>%
  summarize(sdev = sd(FCH4),
            minimun = min(FCH4),
            maximum = max(FCH4),
            mean = mean(FCH4),
            med = median(FCH4)) %>%
  mutate(variable = "FCH4")

FCO2_stats <- f_dat %>% group_by(Origin, Location) %>%
  summarize(sdev = sd(FCO2),
            minimun = min(FCO2),
            maximum = max(FCO2),
            mean = mean(FCO2),
            med = median(FCO2)) %>%
  mutate(variable = "FCO2")




#example graphs of Mitchell's
f_dat %>%
  dplyr::filter(Origin %in% c('g_low','g_mid','g_up'),
                week(timestamp) == 19) %>%
  ggplot(aes(week(timestamp), FCO2, fill = Location)) +
  geom_boxplot(aes(group = interaction(week(timestamp), Location))) +
  facet_grid(.~Origin, labeller = labeller(Origin = origin.labs)) +
  labs(x = "Calendar Week", y = "Flux CO2", title = "Interaction of Origin & Location on CO2") +
  theme_bw()

f_dat %>%
  dplyr::filter( FCH4 < 400 & FCH4 >-5,
                 Origin %in% c('g_low','g_mid','g_up')) %>%
  ggplot(aes(date(timestamp), FCH4, color = SWC)) +
  scale_color_distiller(direction = 1) +
  geom_point() +
  labs(x = "Date", y = "Flux CH4", color = "Soil Moisture") +
  facet_wrap(~Origin) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
