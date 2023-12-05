
#This text creates supplemental results included referred to the discussion and extra materials
#requires objects recreated in GrossFlux.RMD

#see supplement for details of creating conversion factors
#between soil moisture and water-filled pore space

grossFlux$wfps <- grossFlux$sm/1.34
grossFlux[grossFlux$Origin == "g_up",]$wfps <- grossFlux[grossFlux$Origin == "g_up",]$sm/0.64
grossFlux[grossFlux$Origin == "g_mid",]$wfps <- grossFlux[grossFlux$Origin == "g_mid",]$sm/0.99

ggplot(grossFlux, aes(wfps, C_rate)) +
  geom_point(size = 3) +
  geom_smooth(method = lm, formula = y ~ poly(x,2), se = FALSE, size = 0.5) +
  labs(x=  unname(TeX("\n WFPS")),
       y= unname(TeX("\n Consumption $(\\mu mol * g^{-1} * d^{-1})$"))) +
  stat_poly_eq(formula = y ~ poly(x,2),
               aes(label = paste(..eq.label..,
                                 ..adj.rr.label..,
                                 sep = "~~~"))) +
  theme_bw(base_size = 15) +
  theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20))

ggplot(grossFlux, aes(wfps, P_rate)) +
  geom_point(size = 3) +
  geom_smooth(method = lm, formula = y ~ poly(x,2), se = FALSE, size = 0.5) +
  labs(x=  unname(TeX("\n WFPS")),
       y= unname(TeX("\n Production $(\\mu mol * g^{-1} * d^{-1})$"))) +
  stat_poly_eq(formula = y ~ poly(x,2),
               aes(label = paste(..eq.label..,
                                 ..adj.rr.label..,
                                 sep = "~~~"))) +
  theme_bw(base_size = 15) +
  theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20))


