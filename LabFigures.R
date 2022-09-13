
ggplot(data = data, aes(umolPg, umolKg, colour = sm)) +
  geom_point(size = 3) +
  geom_smooth(method = lm, formula = y ~ x) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label..,
                                 ..rr.label..,
                                 sep = "~~~")))

ggplot(data = data, aes(sm, umolPg)) +
  geom_point(aes(color = Origin), size = 3) +
  scale_color_discrete(labels = Olabs) +
  #facet_grid(Location ~ .) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..p.value.label..,
                                 ..eq.label..,
                                 ..rr.label..,
                                 sep = "~~~")))

ggplot(data = data, aes(sm, umolKg, colour = sm)) +
  geom_point(size = 3) +
  facet_grid(Location ~ .) +
  geom_smooth(method = lm, formula = y ~ x) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..p.value.label..,
                                 ..eq.label..,
                                 ..rr.label..,
                                 sep = "~~~")))

ggplot(data = data,
       aes(sm, SWC, colour = as.factor(Collar))) +
  geom_point(size = 3)

ggplot(data = data,
       aes(FCH4, umolPg, colour = as.factor(Collar))) +
  geom_point(size = 3)
