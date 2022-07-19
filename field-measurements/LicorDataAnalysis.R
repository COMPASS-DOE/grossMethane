
anova <- aov(FCH4_dry ~ Origin*Location + date, data = data)
summary(anova)
par(mfrow=c(2,2))
plot(anova)
TukeyHSD(anova)
