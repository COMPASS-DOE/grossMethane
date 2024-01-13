library(ggpubr)

ggplot(l_dat[l_dat$round == "T4",], aes(C_rate, P_rate)) +
  geom_point() + stat_smooth(method = lm)

ggscatter(l_dat[l_dat$round == "T4" ,], x = "P_rate",
          y = "C_rate", add = "reg.line") +
  stat_cor(label.x = 7, label.y = 10) +
  stat_regline_equation(label.x = 7, label.y = 10.5)
