
allData %>%
    filter(final > 1.8,
           dCH4_f > 100) -> trouble

ggplot(trouble, aes(round, delM)) +
    geom_point(aes(color = ap_cor, size = m_cor)) +
    geom_line(aes(group = id))
