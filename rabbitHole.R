
#final concentration and delta value
ggplot(data = allData,
       aes(final, dCH4_f, color = ap_cor)) +
    geom_jitter(aes(shape = Origin), size = 3) +
    scale_shape_discrete(labels = Olabs) +
    ylim(-100,300) + xlim(1,7) +
    ylab("final delta") + xlab("final concentraion")

allData %>%
    arrange(ap_cor) %>%
    filter(final > 1.8,
           dCH4_f > 100) %>%
    mutate(apfit = ifelse(ap_cor > 0.5,
                          "ap_good",
                          "ap_bad")) -> patternT
unique(patternT$id)
patternT_ids <- c("2","69","64","55","60","58",
         "59","44","61","51","41","63")

ggplot(patternT, aes(round, AP_obs)) +
    geom_line(aes(color = id, group = id), size = 0.5) +
    geom_point(aes(color = id), size = 3) +
    facet_grid(apfit~.)

ggplot(patternT, aes(round, Mppm)) +
    geom_line(aes(color = id, group = id), size = 0.5) +
    geom_point(aes(color = id), size = 3) +
    facet_grid(apfit~.)

ggplot(pk_results, aes(k0, k)) +
    geom_abline(slope = 1) +
    geom_point(aes(color = ap_cor), size = 3) +
    ggtitle("All Data")

pk_results %>%
    filter(k < 15,
        k0 < 20) %>%
    arrange(id) -> slopeTs

ggplot(slopeTs, aes(k0, k)) +
    geom_abline(slope = 1) +
    geom_point(aes(color = ap_cor), size = 3) +
    ggtitle("k & k0 < 20")

#remove low k's
slopeT_ids <- unique(slopeTs[slopeTs$k > 5,]$id)

pk_results %>%
    select("id","k", "k0") %>%
    filter(id %in% slopeT_ids) %>%
    left_join(allData, by = "id") %>%
    arrange(id) -> one2one

ggplot(one2one, aes(k0, k)) +
    geom_abline(slope = 1) +
    ylim(5,25) + xlim(5,25) +
    geom_point(aes(color = ap_cor), size = 3) +
    ggtitle("Slope Targets")

pk_results %>%
    select("id","k", "k0") %>%
    filter(id %in% patternT_ids) %>%
    left_join(allData, by = "id") %>%
    arrange(id) %>%
    mutate(apfit = ifelse(ap_cor > 0.5,
                          "ap_good",
                          "ap_bad"),
           group = ifelse(ap_cor > 0.5,
                          "pattern",
                          "both")) -> pattern2one

ggplot(pattern2one, aes(k0, k)) +
    geom_abline(slope = 1) +
    ylim(5,25) + xlim(5,25) +
    geom_point(aes(color = id, shape = apfit), size = 3) +
    ggtitle("Pattern Targets")

pk_results %>%
    select("id","k", "k0") %>%
    filter(id %in% slopeT_ids) %>%
    left_join(allData, by = "id") %>%
    arrange(id) %>%
    mutate(apfit = ifelse(ap_cor > 0.5,
                          "ap_good",
                          "ap_bad"),
           group = ifelse(ap_cor > 0.5,
                          "slope",
                          "both")) -> slope2one

ggplot(slope2one, aes(k0, k)) +
    geom_abline(slope = 1) +
    ylim(5,25) + xlim(5,25) +
    geom_point(aes(color = id, shape = apfit), size = 3) +
    ggtitle("Slope Targets")

# > setdiff(slopeT_ids, patternT_ids)
# [1] "29" "32" "52"
# > setdiff(patternT_ids, slopeT_ids)
# [1] "61" "51"

master_ids <- c("2","29","32","69","64","55","60","58",
            "59","44","61","51","52","41","63")

pk_results %>%
    arrange(ap_cor) %>%
    mutate(apfit = ifelse(ap_cor > 0.5,
                          "ap_good",
                          "ap_bad")) %>%
    filter(id %in% master_ids) %>%
    select("id","k","k0","apfit") %>%
    left_join(allData, by = "id") -> master

master

ggplot(master, aes(round, AP_obs)) +
    geom_line(aes(color = id, group = id), size = 0.5) +
    geom_point(aes(color = id), size = 3) +
    facet_grid(apfit~.)

ggplot(master, aes(round, Mppm)) +
    geom_line(aes(color = id, group = id), size = 0.5) +
    geom_point(aes(color = id), size = 3) +
    facet_grid(apfit~.)

ggplot(master, aes(k0, k)) +
    geom_abline(slope = 1) +
    geom_point(aes(color = dCH4_f,
                   size = final)) +
    geom_text(aes(label = id),
              nudge_y = -0.75,
              check_overlap = T) +
    facet_grid(apfit~.) +
    ggtitle("Slope and Pattern")
