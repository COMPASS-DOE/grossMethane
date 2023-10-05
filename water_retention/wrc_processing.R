## Process and plot water retention curves for GCREW
## KFP, Oct 2023

################ #
################

library(tidyverse)
theme_set(theme_minimal() + theme(text = element_text(size = 11)))


# import and process data ----
import_wrc_data = function(FILEPATH){

  filePaths_wrc <- list.files(path = FILEPATH, pattern = "xlsx", full.names = TRUE, recursive = FALSE)
  wrc_data <- do.call(bind_rows, lapply(filePaths_wrc, function(path) {

    # importing both, the evaluated values and the fitted values
    df_eval <- readxl::read_excel(path, sheet = "Evaluation-Retention Θ(pF)") %>% mutate_all(as.character) %>% janitor::clean_names()
    df_eval = df_eval %>% mutate(source = basename(path)) %>% dplyr::select(p_f, water_content_vol_percent, source) %>% rename(pf_eval = p_f)

    df_fit <- readxl::read_excel(path, sheet = "Fitting-Retention Θ(pF)") %>% mutate_all(as.character) %>% janitor::clean_names()
    df_fit = df_fit %>% mutate(source = basename(path)) %>% dplyr::select(p_f, water_content_vol_percent, source) %>% rename(pf_fit = p_f)

    df <- full_join(df_eval, df_fit)
    df
  }

  ))

}
wrc_data = import_wrc_data(FILEPATH = "water_retention")


process_wrc = function(wrc_data){

  #wrc_processed <-
  wrc_data %>%
    # assign locations
    mutate(location = case_when(grepl("Upland", source) ~ "upland",
                                grepl("Transition", source) ~ "lowland")) %>%
    dplyr::select(location, water_content_vol_percent, starts_with("pf")) %>%
    mutate_at(vars(starts_with("pf")), as.numeric) %>%
    mutate_at(vars(starts_with("water")), as.numeric) %>%
    # convert pF to kPa (water potential units)
    mutate(
           kpa_eval = round((10^pf_eval)/10,2),
           kpa_fit = round((10^pf_fit)/10,2))

}
wrc_processed = process_wrc(wrc_data)

#
# plot the curves ----
wrc_processed %>%
  filter(kpa_fit >= 0 | kpa_eval >= 0) %>%
  filter(pf_fit >= 0 | pf_eval >= 0) %>%
  ggplot(aes(y = water_content_vol_percent, color = location))+
  geom_line(aes(x = kpa_fit), linewidth = 1)+
  geom_point(aes(x = kpa_eval), shape = 1, show.legend = F)+
  scale_x_log10(labels = scales::comma)+
  scale_color_manual(values = c("#FF33CC", "#00CC66"))+
  labs(color = "",
       x = "Water potential (kPa)",
       y = "Volumetric water content (%)")+
  theme(legend.position = c(0.8, 0.8))
#ggsave("water_retention/wrc_fit_and_eval.png", height = 4, width = 4)


wrc_processed %>%
  filter(kpa_fit >= 0 | kpa_eval >= 0) %>%
  filter(pf_fit >= 0 | pf_eval >= 0) %>%
  ggplot(aes(y = water_content_vol_percent, color = location))+
  geom_line(aes(x = kpa_fit), linewidth = 1)+
  scale_x_log10(labels = scales::comma)+
  scale_color_manual(values = c("#FF33CC", "#00CC66"))+
  labs(color = "",
       x = "Water potential (kPa)",
       y = "Volumetric water content (%)")+
  theme(legend.position = c(0.8, 0.8))
#ggsave("water_retention/wrc_fit_only.png", height = 4, width = 4)


