# This code is for analyzing methane flux data from GCREW.
# We are using ANOVA to perform statistical analysis
# for data in PairedMethaneLiCorData.R

# Created on July 20, 2022
# Kendal and Mitchell


anova <- aov(log ~ Origin*Location + date + SWC_mean + TS_mean,
             data = low)
summary(anova)
par(mfrow=c(2,2))
plot(anova)
dev.off()
plot(TukeyHSD(anova, which = "Origin:Location"))

ggplot(low, aes(week(timestamp), FCH4_dry, fill = Origin)) +
    geom_boxplot(aes(group = interaction(week(timestamp), Origin))) +
    labs(x = "Calendar Week", y = "Flux CH4",
         title = "Interaction of Origin & Location on Lowland CH4") +
    theme_bw()
