# This code is for analyzing methane flux data from GCREW.
# We are using ANOVA to perform statistical analysis
# for data in PairedMethaneLiCorData.R

# Created on July 20, 2022
# Kendal and Mitchell

hist(f_dat$FCH4)
hist(log10(f_dat$FCH4 + 20))

anova <- aov(log10(FCH4 + 20) ~ Location*Origin + date + SWC + TS,
             data = f_dat)
summary(anova)
par(mfrow=c(2,2))
plot(anova)
dev.off()
TukeyHSD(anova, which = "Location*Origin")

ggplot(g_low, aes(week(timestamp), FCH4, fill = Origin)) +
    geom_boxplot(aes(group = interaction(week(timestamp), Origin))) +
    labs(x = "Calendar Week", y = "Flux CH4",
         title = "Interaction of Origin & Location on Lowland CH4") +
    theme_bw()


#lowland only
hist(g_low$FCH4)
hist(log10(g_low$FCH4 + 20))

anova_low <- aov(log10(FCH4 + 20) ~ Origin + date + SWC + TS,
             data = g_low)
summary(anova_low)
par(mfrow=c(2,2))
plot(anova_low)
dev.off()
TukeyHSD(anova_low, which = "Origin")

#exclude g_mid
anova_low2 <- aov(log10(FCH4 + 20) ~ Origin + date + SWC + TS,
                 data = g_low[g_low$Origin != "g_mid",])
summary(anova_low2)
par(mfrow=c(2,2))
plot(anova_low2)
dev.off()
TukeyHSD(anova_low2, which = "Origin")
