
###################################################################################
################### code from Bliese & Ployhart 2002 ##############################
###################################################################################

#adapted by AMD

# dataset 'total' has cumulative mass offered as 'value'
# dataset 'total2' has pre3 mass offered as 'value'

# simple regression

lm(d15N.UA~date,data=total,na.action=na.omit)
lm(d15N.UA~date,data=total2,na.action=na.omit)

lm(d15N.UA~value,data=total,na.action=na.omit) # cummass
lm(d15N.UA~value,data=total2,na.action=na.omit) # pre3mass

# no good, because:
# 1. each individual provided >1 piece of information, non-independent
# 2. predictor (time or mass eaten) has a chronological structure
# 3. assumes that all individuals start the same and change at the same rate (i.e., fixed effects)

# intercepts and slopes estimated above are permitted to randomly vary among individuals

## Yij = [B00 + B10(Timeij)] + [u0j + u1j(Timeij) + rij]
## The elements in the brackets on the left are considered the "fixed" portion of the model
## and the elements in the bracket on the right are considered the "random" portion of the model

# At each step, compare loglikelihood ratios (deviances) between models to aid decisions about including specific terms
# assess whether adding more complexity to the model improves model fit above and beyond the existing terms in the model

# Step 1: establish a simple model without any random effects to serve as a baseline (same as above)

model.1.1<-gls(d15N.UA~date,data=total,na.action=na.omit)
model.1.2<-gls(d15N.UA~value,data=total,na.action=na.omit) # cummass
model.1.3<-gls(d15N.UA~value,data=total2,na.action=na.omit) # pre3mass

summary(model.1.1)
summary(model.1.2) # AIC 1534
summary(model.1.3)

# Step 2: add a random effect for ID (random incercepts), but not yet for slope (fixed slopes)

model.2.1<-lme(d15N.UA~date,random=~1|ID,data=total,na.action=na.omit)
model.2.2<-lme(d15N.UA~value,random=~1|ID,data=total,na.action=na.omit)
model.2.3<-lme(d15N.UA~value,random=~1|ID,data=total2,na.action=na.omit)

summary(model.2.1) # p-int: 0; p-date-lin: 0.02
summary(model.2.2) # p-int: 0; p-cummass: 0.18; AIC 1457
summary(model.2.3) # p-int: 0; p-pre3mass: 0.02

# test whether adding random intercept for ID improves model fit (it does)
anova(model.1.1, model.2.1) # p < 0.0001
anova(model.1.2, model.2.2) # p < 0.0001
anova(model.1.3, model.2.3) # p < 0.0001

# Step 3: Add random slopes

ctrl <- lmeControl(opt='optim') # change the new flaky default optimizer for lme back to the old optimizer

#model.3.1<-lme(d15N.UA~date,random=~1+date|ID,data=total,na.action=na.omit) ## TODO: change date from ordered factor to numeric (day 1 = 0)
model.3.2<-lme(d15N.UA~value,random=~1+value|ID,control=ctrl,data=total,na.action=na.omit) # cummass
model.3.3<-lme(d15N.UA~value,random=~1+value|ID,control=ctrl,data=total2,na.action=na.omit) # pre3mass

summary(model.3.2) # p-int: 0; p-cummass: 0; AIC 1454
summary(model.3.3) # p-int: 0; p-pre3mass: 0

anova(model.2.2, model.3.2) # p = 0.0394 *
anova(model.2.3, model.3.3) # p = 0.1062

VarCorr(model.3.3) # negative correlation between intercept and slope indicates that IDs with high initial values tend to have weak slopes, and individuals with low initial values tend to have strong slopes

# Step 4: Add repeated structure

model.4.2a<-update(model.3.2,correlation=corAR1())
model.4.3a<-update(model.3.3,correlation=corAR1())

summary(model.4.2a) # AIC 1450, lowest
summary(model.4.3a)

anova(model.3.2, model.4.2) # p = 0.0189 *
anova(model.3.3, model.4.3a) # p = 0.05

model.4.2b<-update(model.4.2a,weights=varExp(form=~value)) # not working Error in recalc.corAR1(object[[i]], conLin) : NA/NaN/Inf in foreign function call (arg 1)
model.4.3b<-update(model.4.3a,weights=varExp(form=~value))

summary(model.4.3b) # AIC 1439

# Step 5: modeling intercept variation

ctrl <- lmeControl(opt='optim',maxIter=100,msMaxIter=100,niterEM=50,msMaxEval=400)

## TODO: try other vars besides initial BC
## TODO: center bodycon.st around its grand mean

hist(total$bodycon.st)
total$BCst_scaled <- scale(total$bodycon.st, scale=FALSE)
total2$BCst_scaled <- scale(total2$bodycon.st, scale=FALSE)
total$tailtobreak_scaled <- scale(total$tailtobreak, scale=FALSE)

model.5.2<-lme(d15N.UA~value+BCst_scaled,random=~value|ID,
               correlation=corAR1(),na.action=na.omit,data=total,control=ctrl)

summary(model.5.2) # AIC 1452

anova(model.4.2a, model.5.2) # error

model.5.3<-lme(d15N.UA~value+BCst_scaled,random=~value|ID,
               correlation=corAR1(),na.action=na.omit,data=total2,control=ctrl)
summary(model.5.3)

# Step 6: modeling slope variation (add interaction (* instead of +) between value and bodycon.st)

model.6.2<-lme(d15N.UA~value*BCst_scaled,random=~value|ID,
               correlation=corAR1(),na.action=na.omit,data=total,control=ctrl)

summary(model.6.2)

anova(model.5.2, model.6.2) # p = 0.01

model.6.4<-lme(d15N.UA~value*tailtobreak_scaled,random=~value|ID,
               correlation=corAR1(),na.action=na.omit,data=total,control=ctrl)
summary(model.6.4)
