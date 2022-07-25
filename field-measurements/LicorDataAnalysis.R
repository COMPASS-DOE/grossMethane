# This code is for analyzing methane flux data from GCREW.
# We are using ANOVA to perform statistical analysis 
# for data in PairedMethaneLiCorData.R

# Created on July 20, 2022
# Kendal and Mitchell

anova <- aov(FCO2_dry ~ Origin*Location + date + SWC_mean, data = data)
summary(anova)
par(mfrow=c(2,2))
plot(anova)
TukeyHSD(anova)
