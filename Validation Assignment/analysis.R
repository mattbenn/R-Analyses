# Title     : Validation Analysis
# Objective : Finish class project
# Created on: 3/25/2021

setwd("C:\\Users\\matth\\Desktop\\SUNY Albany\\_Spring 2021\\APSY 752.2 - Personnel Psychology\\Validation Assignment")
library("psych")
library("irr")
library("lavaan")
library("semPlot")
library("dominanceanalysis")
#library("tidyverse")
#library("caret")
#library("leaps")
library("olsrr")
library("car")


data <- read.csv("validandselect.csv", header = TRUE)
race <- data$race
neuro <- data.frame(cbind(data$neuro1, data$neuro2, data$neuro3, data$neuro4, data$neuro5, data$neuro6, data$neuro7, data$neuro8, data$neuro9, data$neuro10, data$neuro11, data$neuro12))
extra <- data.frame(cbind(data$extra1, data$extra2, data$extra3, data$extra4, data$extra5, data$extra6, data$extra7, data$extra8, data$extra9, data$extra10, data$extra11, data$extra12))
open <- data.frame(cbind(data$open1, data$open2, data$open3, data$open4, data$open5, data$open6, data$open7, data$open8, data$open9, data$open10, data$open11, data$open12))
agree <- data.frame(cbind(data$agree1, data$agree2, data$agree3, data$agree4, data$agree5, data$agree6, data$agree7, data$agree8, data$agree9, data$agree10, data$agree11, data$agree12))
consc <- data.frame(cbind(data$consc1, data$consc2, data$consc3, data$consc4, data$consc5, data$consc6, data$consc7, data$consc8, data$consc9, data$consc10, data$consc11, data$consc12))
cogtest <- data.frame(data$cogtest)
performance <- data.frame(data$suprate)

colnames(neuro) <- c('neuro1', 'neuro2', 'neuro3', 'neuro4', 'neuro5', 'neuro6', 'neuro7', 'neuro8', 'neuro9', 'neuro10', 'neuro11', 'neuro12')
colnames(extra) <- c('extra1', 'extra2', 'extra3', 'extra4', 'extra5', 'extra6', 'extra7', 'extra8', 'extra9', 'extra10', 'extra11', 'extra12')
colnames(open) <- c('open1', 'open2', 'open3', 'open4', 'open5', 'open6', 'open7', 'open8', 'open9', 'open10', 'open11', 'open12')
colnames(agree) <- c('agree1', 'agree2', 'agree3', 'agree4', 'agree5', 'agree6', 'agree7', 'agree8', 'agree9', 'agree10', 'agree11', 'agree12')
colnames(consc) <- c('consc1', 'consc2', 'consc3', 'consc4', 'consc5', 'consc6', 'consc7', 'consc8', 'consc9', 'consc10', 'consc11', 'consc12')
colnames(cogtest) <- 'cogtest'
colnames(performance) <- 'suprate'

data$neuroMean <- rowMeans(neuro)
data$conscMean <- rowMeans(consc)
data$extraMean <- rowMeans(extra)
data$openMean <- rowMeans(open)
data$agreeMean <- rowMeans(agree)

agreeNew <- subset(agree, select= -c(agree4, agree6, agree11, agree12))
openNew <- subset(open, select= -c(open1, open2, open4, open6, open7, open8, open11, open12))
extraNew <- subset(extra, select= -c(extra3, extra6, extra10, extra12))

agreeNewMean <- rowMeans(agreeNew)
openNewMean <- rowMeans(openNew)
extraNewMean <- rowMeans(extraNew)
data$agreeNewMean <- agreeNewMean
data$openNewMean <- openNewMean
data$extraNewMean <- extraNewMean

# CFA, and then EFA, on the scores

CFAcheck <-
  'neuro =~ neuro1 + neuro2 + neuro3 + neuro4 + neuro5 + neuro6 + neuro7 + neuro8 + neuro9 + neuro10 + neuro11 + neuro12
  extra =~ extra1 + extra2 + extra3 + extra4 + extra5 + extra6 + extra7 + extra8 + extra9 + extra10 + extra11 + extra12
  consc =~ consc1 + consc2 + consc3 + consc4 + consc5 + consc6 + consc7 + consc8 + consc9 + consc10 + consc11 + consc12
  agree =~ agree1 + agree2 + agree3 + agree4 + agree5 + agree6 + agree7 + agree8 + agree9 + agree10 + agree11 + agree12
  open =~ open1 + open2 + open3 + open4 + open5 + open6 + open7 + open8 + open9 + open10 + open11 + open12
  neuro~~extra
  neuro~~consc
  neuro~~agree
  neuro~~open
  extra~~consc
  extra~~agree
  extra~~open
  consc~~agree
  consc~~open
  agree~~open
  '


justScores <- data.frame(cbind(neuro, consc, extra, open, agree))

CFAfit <- cfa(CFAcheck, data=justScores, std.lv=TRUE, orthogonal=FALSE, missing="listwise")
summary(CFAfit, standardized=TRUE, fit.measure=TRUE)

fa.parallel(justScores, fm="ml", fa="both", main="Big 5 Check", n.iter=1000, SMC=TRUE, show.legend=TRUE)
FAcheck <- fa(justScores, nfactors=4, rotate="oblimin", SMC=TRUE, max.iter=1000, fm="ml", alpha=.05)
print(FAcheck, sort=TRUE)

FAcheck <- fa(justScores, nfactors=5, rotate="oblimin", SMC=TRUE, max.iter=1000, fm="ml", alpha=.05)
print(FAcheck, sort=TRUE)

# The fit statistics for the model were unacceptably low, so EFA was performed to find the best items to cut

neuro2 <- data.frame(cbind(data$neuro2, data$neuro3, data$neuro4, data$neuro5, data$neuro6, data$neuro7, data$neuro8, data$neuro9, data$neuro11, data$neuro12))
extra2 <- data.frame(cbind(data$extra1, data$extra2, data$extra4, data$extra5, data$extra7, data$extra8, data$extra10, data$extra11))
open2 <- data.frame(cbind(data$open3, data$open5, data$open9))
agree2 <- data.frame(cbind(data$agree4, data$agree8))
consc2 <- data.frame(cbind(data$consc1, data$consc2, data$consc3, data$consc4, data$consc5, data$consc7, data$consc8, data$consc10, data$consc11, data$consc12))

colnames(neuro2) <- c('neuro2', 'neuro3', 'neuro4', 'neuro5', 'neuro6', 'neuro7', 'neuro8', 'neuro9', 'neuro11', 'neuro12')
colnames(extra2) <- c('extra1', 'extra2', 'extra4', 'extra5', 'extra7', 'extra8', 'extra10', 'extra11')
colnames(open2) <- c('open3', 'open5', 'open9')
colnames(agree2) <- c('agree4', 'agree8')
colnames(consc2) <- c('consc1', 'consc2', 'consc3', 'consc4', 'consc5', 'consc7', 'consc8', 'consc10', 'consc11', 'consc12')

justScores2 <- cbind(neuro2, extra2, open2, agree2, consc2)

fa.parallel(justScores2, fm="ml", fa="both", main="Big 5 Check", n.iter=1000, SMC=TRUE, show.legend=TRUE)
FAcheck2 <- fa(justScores2, nfactors=5, rotate="oblimin", SMC=TRUE, max.iter=1000, fm="ml", alpha=.05)
print(FAcheck2, sort=TRUE)

CFAcheck2 <-
  'neuro =~ neuro2 + neuro3 + neuro4 + neuro5 + neuro6 + neuro7 + neuro8 + neuro9 + neuro11 + neuro12
  extra =~ extra1 + extra2 + extra4 + extra5 + extra7 + extra8 + extra10 + extra11
  consc =~ consc1 + consc2 + consc3 + consc4 + consc5 + consc7 + consc8 + consc10 + consc11 + consc12
  agree =~ agree4 + agree8
  open =~ open3 + open5 + open9
  neuro~~extra
  neuro~~consc
  neuro~~agree
  neuro~~open
  extra~~consc
  extra~~agree
  extra~~open
  consc~~agree
  consc~~open
  agree~~open
  '

CFAfit2 <- cfa(CFAcheck2, data=justScores2, std.lv=TRUE, orthogonal=FALSE)
summary(CFAfit2, standardized=TRUE, fit.measure=TRUE)

# Reliability: Whole test

glb(neuro)
psych::alpha(neuro, check.keys = TRUE)
glb(consc)
psych::alpha(consc, check.keys = TRUE)
glb(agree)
psych::alpha(agree, check.keys = TRUE)
glb(open)
psych::alpha(open, check.keys = TRUE)
glb(extra)
psych::alpha(extra, check.keys = TRUE)

print(fa(agree, nfactors=1, rotate="oblimin", SMC=TRUE, max.iter=1000, fm="ml", alpha=0.5), sort=TRUE)
print(fa(consc, nfactors=1, rotate="oblimin", SMC=TRUE, max.iter=1000, fm="ml", alpha=0.5), sort=TRUE)
print(fa(neuro, nfactors=1, rotate="oblimin", SMC=TRUE, max.iter=1000, fm="ml", alpha=0.5), sort=TRUE)
print(fa(open, nfactors=1, rotate="oblimin", SMC=TRUE, max.iter=1000, fm="ml", alpha=0.5), sort=TRUE)
print(fa(extra, nfactors=1, rotate="oblimin", SMC=TRUE, max.iter=1000, fm="ml", alpha=0.5), sort=TRUE)

neuroNew <- subset(neuro, select= -neuro10)
agreeNew <- subset(agree, select= -c(agree4, agree6, agree11, agree12))
openNew <- subset(open, select= -c(open1, open2, open4, open6, open7, open8, open11, open12))
extraNew <- subset(extra, select= -c(extra3, extra6, extra10, extra12))
conscNew <- subset(consc, select= -consc6)

glb(neuroNew)
alpha(neuroNew, check.keys = TRUE)
glb(conscNew)
alpha(conscNew, check.keys = TRUE)
glb(agreeNew)
alpha(agreeNew, check.keys = TRUE)
glb(openNew)
alpha(openNew, check.keys = TRUE)
glb(extraNew)
alpha(extraNew, check.keys = TRUE)

### Descriptives for original and shortened scales

neuroMean <- rowMeans(neuro)
conscMean <- rowMeans(consc)
agreeMean <- rowMeans(agree)
openMean <- rowMeans(open)
extraMean <- rowMeans(extra)
agreeNewMean <- rowMeans(agreeNew)
openNewMean <- rowMeans(openNew)
extraNewMean <- rowMeans(extraNew)

data$agreeNewMean <- agreeNewMean
data$openNewMean <- openNewMean
data$extraNewMean <- extraNewMean

originalMeans <- data.frame(neuroMean, conscMean, agreeMean, openMean, extraMean)
usedScaleMeans <- data.frame(neuroMean, conscMean, agreeNewMean, openNewMean, extraNewMean)
allMeans <- data.frame(neuroMean, conscMean, agreeMean, openMean, extraMean, agreeNewMean, openNewMean, extraNewMean)

describe(allMeans)
describe(performance) # performance highly skewed
cor(originalMeans, use="complete.obs")
cor(usedScaleMeans, use="complete.obs")

# Testing Normality
data$suprateXform <- log10(6-(data$suprate)) # Correct transformation of supervisor ratings--need to use this instead!!
describe(data$suprateXform)

hist(data$suprate, main="", xlab = "Supervisor Rating Score")
hist(data$suprateXform, main="", xlab = "Transformed Supervisor Rating Score")
hist(data$conscMean, main="", xlab = "Conscientiousness")
hist(data$neuroMean, main="", xlab = "Neuroticism")
hist(data$agreeNewMean, main="", xlab = "Agreeability")
hist(data$openNewMean, main="", xlab = "Openness")
hist(data$extraNewMean, main="", xlab = "Extraversion")

qqPlot(data$suprate, xlab = "Theorized Quantiles", ylab = "Observed Quantiles", main="Supervisor Ratings")
qqPlot(data$suprateXform, xlab = "Theorized Quantiles", ylab = "Observed Quantiles", main="Supervisor Ratings (Transformed)")
qqPlot(data$conscMean, xlab = "Theorized Quantiles", ylab = "Observed Quantiles", main="Conscientiousness")
qqPlot(data$neuroMean, xlab = "Theorized Quantiles", ylab = "Observed Quantiles", main="Neuroticism")
qqPlot(data$agreeMean, xlab = "Theorized Quantiles", ylab = "Observed Quantiles", main="Agreeableness")
qqPlot(data$openMean, xlab = "Theorized Quantiles", ylab = "Observed Quantiles", main="Openness to Experience")
qqPlot(data$extraMean, xlab = "Theorized Quantiles", ylab = "Observed Quantiles", main="Extraversion")
qqPlot(data$extraNewMean, xlab = "Theorized Quantiles", ylab = "Observed Quantiles", main="Extraversion (Altered)")
qqPlot(data$agreeNewMean, xlab = "Theorized Quantiles", ylab = "Observed Quantiles", main="Agreeableness (Altered)")
qqPlot(data$openNewMean, xlab = "Theorized Quantiles", ylab = "Observed Quantiles", main="Openness to Experience (Altered)")

cor.test(neuroMean, conscMean)
cor.test(neuroMean, agreeMean)
cor.test(neuroMean, openMean)
cor.test(neuroMean, extraMean)
cor.test(neuroMean, agreeNewMean)
cor.test(neuroMean, openNewMean)
cor.test(neuroMean, extraNewMean)
cor.test(conscMean, agreeMean)
cor.test(conscMean, openMean)
cor.test(conscMean, extraMean)
cor.test(conscMean, agreeNewMean)
cor.test(conscMean, openNewMean)
cor.test(conscMean, extraNewMean)
cor.test(agreeMean, openMean)
cor.test(agreeMean, extraMean)
cor.test(agreeMean, agreeNewMean)
cor.test(agreeMean, openNewMean)
cor.test(agreeMean, extraNewMean)
cor.test(openMean, extraMean)
cor.test(openMean, agreeNewMean)
cor.test(openMean, openNewMean)
cor.test(openMean, extraNewMean)
cor.test(extraMean, agreeNewMean)
cor.test(extraMean, openNewMean)
cor.test(extraMean, extraNewMean)
cor.test(agreeNewMean, openNewMean)
cor.test(agreeNewMean, extraNewMean)
cor.test(openNewMean, extraNewMean)

# Correlation b/t scales and performance ratings
cor.test(neuroMean, data$suprateXform)
cor.test(conscMean, data$suprateXform)
cor.test(agreeMean, data$suprateXform)
cor.test(openMean, data$suprateXform)
cor.test(extraMean, data$suprateXform)
cor.test(agreeNewMean, data$suprateXform)
cor.test(openNewMean, data$suprateXform)
cor.test(extraNewMean, data$suprateXform)

### Regression Assumptions
library("tidyverse")
library("broom")
library("ggplot2")
library("GGally")
library("car")
library("MASS")

conscReg <- lm(data$suprate ~ data$conscMean, data=data)
extraNewReg <- lm(data$suprate ~ data$extraNewMean, data=data)
neuroReg <- lm(data$suprate ~ data$neuroMean, data=data)

data$suprateXform <- log10(6-(data$suprate)) # Correct transformation of supervisor ratings--need to use this instead!!
consc.Reg <- lm(data$suprateXform ~ data$conscMean, data)
extraNew.Reg <- lm(data$suprateXform ~ data$extraNewMean, data)
neuro.Reg <- lm(data$suprateXform ~ data$neuroMean, data)
agreeNew.Reg <- lm(data$suprateXform ~ data$agreeNewMean, data)
openNew.Reg <- lm(data$suprateXform ~ data$openNewMean, data)
final.Reg <- lm(data$suprateXform ~ data$conscMean + data$neuroMean + data$openNewMean + data$extraNewMean, data)
semifinal.Reg <- lm(data$suprateXform ~ data$conscMean + data$neuroMean + data$openNewMean, data) # Final.reg is good enough

# Homoscedasticity
spreadLevelPlot(consc.Reg, main="Conscientiousness") # checks for homoscedacity by comparing fitted values to absolute studentized residuals
spreadLevelPlot(extraNew.Reg, main="Extraversion")
spreadLevelPlot(neuro.Reg, main="Neuroticism")
spreadLevelPlot(openNew.Reg, main="Openness to Experience")
spreadLevelPlot(agreeNew.Reg, main="Agreeableness")
spreadLevelPlot(final.Reg, main="All Predictors")
spreadLevelPlot(semifinal.Reg, main="Almost All Predictors")

# Bivariate Normality
qqPlot(consc.Reg, xlab = "Theorized Quantiles", ylab = "Studentized Residuals", main="")
qqPlot(neuro.Reg, xlab = "Theorized Quantiles", ylab = "Studentized Residuals", main="")
qqPlot(extraNew.Reg, xlab = "Theorized Quantiles", ylab = "Studentized Residuals", main="")
qqPlot(openNew.Reg, xlab = "Theorized Quantiles", ylab = "Studentized Residuals", main="")
qqPlot(agreeNew.Reg, xlab = "Theorized Quantiles", ylab = "Studentized Residuals", main="")


# Linearity
ggpairs(consc.Reg, columns=c(2,7:5)) # this will create a scatterplot matrix of X variables against Y
hist(cooks.distance(consc.Reg), main="Conscientiousness", xlab="Cook's D")
ggpairs(extraNew.Reg, columns=c(2,7:5)) # this will create a scatterplot matrix of X variables against Y
hist(cooks.distance(extraNew.Reg), main="Extraversion", xlab="Cook's D")
ggpairs(neuro.Reg, columns=c(2,7:5)) # this will create a scatterplot matrix of X variables against Y
hist(cooks.distance(neuro.Reg), main="Neuroticism", xlab="Cook's D")
ggpairs(openNew.Reg, columns=c(2,7:5)) # this will create a scatterplot matrix of X variables against Y
hist(cooks.distance(openNew.Reg), main="Openness", xlab="Cook's D")
ggpairs(agreeNew.Reg, columns=c(2,7:5)) # this will create a scatterplot matrix of X variables against Y
hist(cooks.distance(agreeNew.Reg), main="Agreeability", xlab="Cook's D")

ggpairs(final.Reg, columns=c(9:7))

# Multicollinearity
vif(final.Reg) # Gives VIF
1/vif(final.Reg) # Gives Tolerance


#### Linear Regression: How does each predictor relate to supervisor ratings?
library("QuantPsyc")
neuroReg <- lm(data$suprateXform ~ data$neuroMean, data=data)
conscReg <- lm(data$suprateXform ~ data$conscMean, data=data)
agreeReg <- lm(data$suprateXform ~ data$agreeMean, data=data)
openReg <- lm(data$suprateXform ~ data$openMean, data=data)
extraReg <- lm(data$suprateXform ~ data$extraMean, data=data)

agreeNewReg <- lm(data$suprateXform ~ data$agreeNewMean, data=data)
openNewReg <- lm(data$suprateXform ~ data$openNewMean, data=data)
extraNewReg <- lm(data$suprateXform ~ data$extraNewMean, data=data)

summary(neuroReg) # Summary (reverse connection, b/c sign reversed)
lm.beta(neuroReg) # Gives standardized coefficient
summary(conscReg)
lm.beta(conscReg)
summary(agreeReg)
lm.beta(agreeReg)
summary(openReg)
lm.beta(openReg)
summary(extraReg)
lm.beta(extraReg)
summary(agreeNewReg)
lm.beta(agreeNewReg)
summary(openNewReg)
lm.beta(openNewReg)
summary(extraNewReg)
lm.beta(extraNewReg)

data2 <- data
allReg <- lm(data$suprateXform ~ data$neuroMean + data$conscMean + data$agreeNewMean + data$openNewMean + data$extraNewMean, data2)
summary(allReg)

# Dominance Analysis
da <- dominanceAnalysis(allReg)
summary(da)

# Stepwise Regression (doesn't work with this code)
subsets <- lm(data$suprateXform ~ data$neuroMean + data$conscMean + data$agreeNewMean +
  data$openNewMean + data$extraNewMean, data=data)
ols_step_all_possible(subsets) # All possible subsets
ols_step_best_subset(subsets) # Best subsets according to different criterion
plot(ols_step_forward_aic(subsets, details = TRUE)) # Stepwise Forward Regression
plot(ols_step_backward_aic(subsets, details = TRUE)) # Stepwise Backward
plot(ols_step_forward_p(subsets, details = TRUE)) # Stepwise Forward Regression
ols_step_backward_p(subsets, details = TRUE) # Stepwise Backward
ols_step_both_p(subsets, details = TRUE) # Both forward and backward


models <- regsubsets(data$suprate ~ data$neuroMean + data$conscMean + data$agreeNewMean +
  data$openNewMean + data$extraNewMean, data=data, nvmax=5, method="forward", matrix.logical=FALSE)
summary(models)

library("olsrr")

# Testing nested models
nestedReg <- lm(data$suprateXform ~ data$conscMean + data$neuroMean + data$openNewMean, data)
nestorReg <- lm(data$suprateXform ~ data$conscMean + data$neuroMean + data$openNewMean + data$extraNewMean, data)
anova(nestedReg, nestorReg)

summary(nestedReg)
summary(nestorReg)

CEAReg <- lm(data$suprate ~ data$conscMean + data$extraNewMean + data$agreeNewMean, data)
summary(CEAReg)
anova(nestedReg, CEAReg)

anova(conscReg, nestedReg)

# Final regression models: Models 1, 2, and 3
#finalReg <- lm(data$suprateXform ~ data$conscMean + data$neuroMean + data$openNewMean + data$agreeNewMean, data)
ModelOne.Reg <- lm(data$suprateXform ~ data$conscMean + data$neuroMean, data)
ModelTwo.Reg <- lm(data$suprateXform ~ data$conscMean + data$neuroMean + data$openNewMean, data)
ModelThree.Reg <- lm(data$suprateXform ~ data$conscMean + data$neuroMean + data$openNewMean + data$extraNewMean, data)
summary(ModelOne.Reg, r.squared=TRUE, coefficients=TRUE, correlation=TRUE, adj.r=TRUE)
lm.beta(ModelOne.Reg)
lm.beta(ModelOne.Reg)^2
vif(ModelOne.Reg)
1/vif(ModelOne.Reg)

summary(ModelTwo.Reg, r.squared=TRUE, coefficients=TRUE, correlation=TRUE, adj.r=TRUE)
lm.beta(ModelTwo.Reg)
lm.beta(ModelTwo.Reg)^2
vif(ModelTwo.Reg)
1/vif(ModelTwo.Reg)


summary(ModelThree.Reg, r.squared=TRUE, coefficients=TRUE, correlation=TRUE, adj.r=TRUE)
lm.beta(ModelThree.Reg)
lm.beta(ModelThree.Reg)^2
vif(ModelThree.Reg)
1/vif(ModelThree.Reg)

ModelFour.Reg <- lm(data$suprateXform ~ data$conscMean + data$openNewMean + data$extraNewMean, data)
summary(ModelFour.Reg, r.squared=TRUE, coefficients=TRUE, correlation=TRUE, adj.r=TRUE)
lm.beta(ModelFour.Reg)
lm.beta(ModelFour.Reg)^2
vif(ModelFour.Reg)
1/vif(ModelFour.Reg)

ModelFive.Reg <- lm(data$suprateXform ~ data$neuroMean + data$openNewMean + data$extraNewMean, data)
summary(ModelFive.Reg, r.squared=TRUE, coefficients=TRUE, correlation=TRUE, adj.r=TRUE)
lm.beta(ModelFive.Reg)
lm.beta(ModelFive.Reg)^2
vif(ModelFive.Reg)
1/vif(ModelFive.Reg)

#neuroMean <- rowMeans(neuro)
#conscMean <- rowMeans(consc)
#agreeMean <- rowMeans(agree)
#openMean <- rowMeans(open)
#extraMean <- rowMeans(extra)
#agreeNewMean <- rowMeans(agreeNew)
#openNewMean <- rowMeans(openNew)
#extraNewMean <- rowMeans(extraNew)

#####################################################
#####################################################
#####################################################
#####################################################
# Cleary Test
# neuro, extra, open, agree, consc, cogtest, openNew, ExtraNew; regress on performance
# Hierarchical regression: need to create interaction terms for each predictor X gender, and gender

library("QuantPsyc")
ClearyData <- subset(data, select=-c(X, X.1))

## Mean centering (makes moderation analysis easier)
ClearyData$neuroMean <- ClearyData$neuroMean - mean(ClearyData$neuroMean, na.rm=TRUE)
ClearyData$extraMean <- ClearyData$extraMean - mean(ClearyData$extraMean, na.rm=TRUE)
ClearyData$openMean <- ClearyData$openMean - mean(ClearyData$openMean, na.rm=TRUE)
ClearyData$agreeMean <- ClearyData$agreeMean - mean(ClearyData$agreeMean, na.rm=TRUE)
ClearyData$conscMean <- ClearyData$conscMean - mean(ClearyData$conscMean, na.rm=TRUE)
ClearyData$cogtest <- ClearyData$cogtest - mean(ClearyData$cogtest, na.rm=TRUE)
ClearyData$openNew <- ClearyData$openNew - mean(ClearyData$openNew, na.rm=TRUE)
ClearyData$extraNew <- ClearyData$extraNew - mean(ClearyData$extraNew, na.rm=TRUE)
ClearyData$suprate <- ClearyData$suprate - mean(ClearyData$suprate, na.rm=TRUE)

ClearyData$race_neuroMean <- ClearyData$race * ClearyData$neuroMean
ClearyData$race_extraMean <- ClearyData$race * ClearyData$extraMean
ClearyData$race_openMean <- ClearyData$race * ClearyData$openMean
ClearyData$race_agreeMean <- ClearyData$race * ClearyData$agreeMean
ClearyData$race_conscMean <- ClearyData$race * ClearyData$conscMean
ClearyData$race_cogtest <- ClearyData$race * ClearyData$cogtest
ClearyData$race_openNewMean <- ClearyData$race * ClearyData$openNewMean
ClearyData$race_extraNewMean <- ClearyData$race * ClearyData$extraNewMean

# Regression Equations
raceAndNeuro <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$neuroMean)
raceXneuro <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$neuroMean + ClearyData$race_neuroMean)

raceAndExtra <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$extraMean)
raceXextra <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$extraMean + ClearyData$race_extraMean)

raceAndOpen <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$openMean)
raceXopen <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$openMean + ClearyData$race_openMean)

raceAndAgree <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$agreeMean)
raceXagree <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$agreeMean + ClearyData$race_agreeMean)

raceAndConsc <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$conscMean)
raceXconsc <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$conscMean + ClearyData$race_conscMean)

raceAndCogtest <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$cogtest)
raceXcogtest <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$cogtest + ClearyData$race_cogtest)

raceAndOpenNew <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$openNewMean)
raceXopenNew <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$openNewMean + ClearyData$race_openNewMean)

raceAndExtraNew <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$extraNewMean)
raceXextraNew <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$extraNewMean + ClearyData$race_extraNewMean)

raceReg <- lm(ClearyData$suprate ~ ClearyData$race)

# Summaries!
summary(raceReg) # Race barely sig (b = -0.260; p = .0575)
summary(raceAndNeuro) # Neuro sig; race not
summary(raceXneuro) # Neuro less sig; race and interaction not
summary(raceAndExtra) # Extra sig; race not
summary(raceXextra) # Extra less sig; race and interaction not
summary(raceAndOpen) # Race more sig than by itself; Open not sig
summary(raceXopen) # Race still sig; open and int are not
summary(raceAndAgree) # Agree sig, race not
summary(raceXagree) # Agree sig, race not, interaction marginally (p = .09)
summary(raceAndConsc) # Consc sig, race not
summary(raceXconsc) # Consc sig, interaction sig, race not at all

raceConscReg <- lm(ClearyData$race ~ ClearyData$conscMean)
summary(raceConscReg) # Race and consc have a clear connection (p < .001)

summary(raceAndCogtest) # Cogtest not sig; race marginally so (p = .05) TODO Why?
summary(raceXcogtest) # Nothing is significant

1/vif(raceAndCogtest) # Almost no covariance b/t race and cognitive test
1/vif(raceXcogtest) # HUGE overlap b/t interaction and cogtest, but almost none b/t interaction and race
raceCogtestReg <- lm(ClearyData$race ~ ClearyData$cogtest)
summary(raceCogtestReg) # Very weak correlation b/t race and cognitive test

summary(raceAndOpenNew) # Open not sig; race marginally so (p = .09)
summary(raceXopenNew) # openness marginally sig (p = .09) TODO Why the reversal here?
summary(raceAndExtraNew) # Extra sig; race not
summary(raceXextraNew) # Extra sig; race and interaction not

#### Plot regression lines
devtools::install_github("cardiomoon/ggiraphExtra")
# https://cran.r-project.org/web/packages/ggiraphExtra/vignettes/ggPredict.html
library(ggiraph)
library(ggiraphExtra, dependencies=TRUE)
library(plyr)
library(ggplot2)
ggpredict(raceAndOpenNew, colorAsFactor = TRUE, interactive=TRUE) # Doesn't work
ggplot(raceAndOpenNew)

equation1 <- function(x){coef(raceAndOpenNew)[2]*x+coef(raceAndOpenNew)[1]}
equation2 <- function(x){coef(raceAndOpenNew)[2]*x+coef(raceAndOpenNew)[1]+coef(raceAndOpenNew)[3]}

ggplot(ClearyData, aes(y=suprate, x=openNewMean, color=race))+geom_point()+
  stat_function(fun=equation1,geom="line",color=scales::hue_pal()(2)[1])+
  stat_function(fun=equation2,geom="line",color=scales::hue_pal()(2)[2])

install.packages("ggeffects")
library(ggeffects)
library(sjmisc)

ggpredict(raceAndOpenNew, terms="openNewMean")
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
# Part 2, with 'correct' scales
# Instructions: Only drop items 2 and 6 for Openness and create the compositive measure with the remaining items
library("psych")
library("irr")
library("lavaan")
library("semPlot")
library("dominanceanalysis")
#library("tidyverse")
#library("caret")
#library("leaps")
library("olsrr")
library("car")
library("QuantPsyc")
data <- read.csv("validandselect.csv", header = TRUE)
race <- data$race
neuro <- data.frame(cbind(data$neuro1, data$neuro2, data$neuro3, data$neuro4, data$neuro5, data$neuro6, data$neuro7, data$neuro8, data$neuro9, data$neuro10, data$neuro11, data$neuro12))
extra <- data.frame(cbind(data$extra1, data$extra2, data$extra3, data$extra4, data$extra5, data$extra6, data$extra7, data$extra8, data$extra9, data$extra10, data$extra11, data$extra12))
open <- data.frame(cbind(data$open1, data$open3, data$open4, data$open5, data$open7, data$open8, data$open9, data$open10, data$open11, data$open12))
agree <- data.frame(cbind(data$agree1, data$agree2, data$agree3, data$agree4, data$agree5, data$agree6, data$agree7, data$agree8, data$agree9, data$agree10, data$agree11, data$agree12))
consc <- data.frame(cbind(data$consc1, data$consc2, data$consc3, data$consc4, data$consc5, data$consc6, data$consc7, data$consc8, data$consc9, data$consc10, data$consc11, data$consc12))
cogtest <- data.frame(data$cogtest)
performance <- data.frame(data$suprate)

colnames(neuro) <- c('neuro1', 'neuro2', 'neuro3', 'neuro4', 'neuro5', 'neuro6', 'neuro7', 'neuro8', 'neuro9', 'neuro10', 'neuro11', 'neuro12')
colnames(extra) <- c('extra1', 'extra2', 'extra3', 'extra4', 'extra5', 'extra6', 'extra7', 'extra8', 'extra9', 'extra10', 'extra11', 'extra12')
colnames(open) <- c('open1', 'open3', 'open4', 'open5', 'open7', 'open8', 'open9', 'open10', 'open11', 'open12')
colnames(agree) <- c('agree1', 'agree2', 'agree3', 'agree4', 'agree5', 'agree6', 'agree7', 'agree8', 'agree9', 'agree10', 'agree11', 'agree12')
colnames(consc) <- c('consc1', 'consc2', 'consc3', 'consc4', 'consc5', 'consc6', 'consc7', 'consc8', 'consc9', 'consc10', 'consc11', 'consc12')
colnames(cogtest) <- 'cogtest'
colnames(performance) <- 'suprate'

data$neuroMean <- rowMeans(neuro)
data$conscMean <- rowMeans(consc)
data$extraMean <- rowMeans(extra)
data$openMean <- rowMeans(open)
data$agreeMean <- rowMeans(agree)

#######################################################
# Cleary Test
# Cleary Test
# neuro, extra, open, agree, consc, cogtest, openNew, ExtraNew; regress on performance
# Hierarchical regression: need to create interaction terms for each predictor X gender, and gender

ClearyData <- subset(data, select=-c(X, X.1))

## Mean centering (makes moderation analysis easier)
ClearyData$neuroMean <- ClearyData$neuroMean - mean(ClearyData$neuroMean, na.rm=TRUE)
ClearyData$extraMean <- ClearyData$extraMean - mean(ClearyData$extraMean, na.rm=TRUE)
ClearyData$openMean <- ClearyData$openMean - mean(ClearyData$openMean, na.rm=TRUE)
ClearyData$agreeMean <- ClearyData$agreeMean - mean(ClearyData$agreeMean, na.rm=TRUE)
ClearyData$conscMean <- ClearyData$conscMean - mean(ClearyData$conscMean, na.rm=TRUE)
ClearyData$cogtest <- ClearyData$cogtest - mean(ClearyData$cogtest, na.rm=TRUE)
# ClearyData$suprate <- ClearyData$suprate - mean(ClearyData$suprate, na.rm=TRUE) # Don't need to mean center Y

ClearyData$race_neuroMean <- ClearyData$race * ClearyData$neuroMean
ClearyData$race_extraMean <- ClearyData$race * ClearyData$extraMean
ClearyData$race_openMean <- ClearyData$race * ClearyData$openMean
ClearyData$race_agreeMean <- ClearyData$race * ClearyData$agreeMean
ClearyData$race_conscMean <- ClearyData$race * ClearyData$conscMean
ClearyData$race_cogtest <- ClearyData$race * ClearyData$cogtest

# Regression Equations
raceAndNeuro <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$neuroMean)
raceXneuro <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$neuroMean + ClearyData$race_neuroMean)

raceAndExtra <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$extraMean)
raceXextra <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$extraMean + ClearyData$race_extraMean)

raceAndOpen <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$openMean)
raceXopen <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$openMean + ClearyData$race_openMean)

raceAndAgree <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$agreeMean)
raceXagree <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$agreeMean + ClearyData$race_agreeMean)

raceAndConsc <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$conscMean)
raceXconsc <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$conscMean + ClearyData$race_conscMean)

raceAndCogtest <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$cogtest)
raceXcogtest <- lm(ClearyData$suprate ~ ClearyData$race + ClearyData$cogtest + ClearyData$race_cogtest)

raceReg <- lm(ClearyData$suprate ~ ClearyData$race)
cogReg <- lm(ClearyData$suprate ~ ClearyData$cogtest)

# Summaries!
summary(raceReg) # est = -0.259; p = .0575
lm.beta(raceReg)
summary(cogReg) # est = -0.0088; p = .514
lm.beta(cogReg)
summary(raceAndNeuro) # Race: est = -0.09, p = 0.48; Neuro: est = -.397, p < .001
lm.beta(raceAndNeuro) # For standardized coefficient
summary(raceXneuro) # Race: est = -0.113, p = 0.39; Neuro: est = -0.73, p = 0.0103; Int: est = 0.24, p = 0.21
lm.beta(raceXneuro)
summary(raceAndExtra) # Race: est = -.127, p = 0.33; Extra: est = 0.453, p < .001
lm.beta(raceAndExtra)
summary(raceXextra) # Race: est = -.139, p = .296; Extra: est = 0.908, p = .012; Int: est = -.315, p = 0.178
lm.beta(raceXextra)
summary(raceAndOpen) # Race: est = -.341, p = 0.0099; Open: est = -0.03, p = 0.795
lm.beta(raceAndOpen)
summary(raceXopen) # Race: est = -.337, p = .011; Open: est = -.3007, p = .434; Int: est = 0.185, p = 0.46
lm.beta(raceXopen)
summary(raceAndAgree) # Race: est = -.093, p = 0.509; Agree: est = .443, p = .0021
lm.beta(raceAndAgree)
summary(raceXagree) # Race: est = -.1, p = .438; Agree: est = 1.15, p < .01; Int: est = -.477, p = .091
lm.beta(raceXagree)
summary(raceAndConsc) # Race: est = .069, p = .601; Consc: est = .610, p < .001
lm.beta(raceAndConsc)
summary(raceXconsc) # Race: est = .007, p = .957; Consc: est = 1.558, p < .001; Int: est = -.687, p = .002
lm.beta(raceXconsc)

1/vif(raceAndConsc)
1/vif(raceXconsc)

raceConscReg <- lm(ClearyData$race ~ ClearyData$conscMean)
summary(raceConscReg) # est: -.336, p < .001
lm.beta(raceConscReg)


summary(raceAndCogtest) # Race: est = -.26, p = .0542; Cog: est = -.010, p = .458
lm.beta(raceAndCogtest)
summary(raceXcogtest) # Race: est = -.262, p = .0548; Cog: est = -.0003, p = .993; Int: est = -.006, p = .808
lm.beta(raceXcogtest)

1/vif(raceAndCogtest) # Tol = .998
1/vif(raceXcogtest) # RaceTol = .998, CogRol = .103, IntTol = .103
raceCogtestReg <- lm(ClearyData$race ~ ClearyData$cogtest)
summary(raceCogtestReg) # est = -.004, p = .624

#################################################
# Tables Work
# Documentation: https://cran.r-project.org/web/packages/apaTables/apaTables.pdf
devtools::install_github("")
install.packages("apaTables", dep=T)
library("apaTables")
apa.reg.table(raceAndConsc, filename="testTable.doc")
apa.reg.table(raceXconsc, filename="testTable.doc", table.number = 1)


#### Plot regression lines
devtools::install_github("cardiomoon/ggiraphExtra")
# https://cran.r-project.org/web/packages/ggiraphExtra/vignettes/ggPredict.html
library(ggiraph)
library(ggiraphExtra, dependencies=TRUE)
library(plyr)
library(ggplot2)
ggpredict(raceAndOpenNew, colorAsFactor = TRUE, interactive=TRUE) # Doesn't work
ggplot(raceAndOpen)

equation1 <- function(x){coef(raceAndConsc)[2]*x+coef(raceAncConsc)[1]}
equation2 <- function(x){coef(raceXconsc)[2]*x+coef(raceXconsc)[1]+coef(raceXconsc)[3]}

cdata_r1 <- ClearyData[which(race==1),]
cdata_r2 <- ClearyData[which(race==2),]

raceConsc1 <- lm(cdata_r1$suprate ~ cdata_r1$conscMean)
summary(raceConsc1)
raceConsc2 <- lm(cdata_r2$suprate ~ cdata_r2$conscMean)
summary(raceConsc2)

equation3 <- function(x){coef(raceConsc1)[1] + coef(raceConsc1)[2]*x}
equation4 <- function(x){coef(raceConsc2)[1] + coef(raceConsc2)[2]*x}

ggplot(ClearyData, aes(y=suprate, x=conscMean, color=race))+geom_point()+
  stat_function(fun=equation1,geom="line",color=scales::hue_pal()(2)[1])+
  stat_function(fun=equation2,geom="line",color=scales::hue_pal()(2)[2])

ggplot(ClearyData, aes(y=suprate, x=conscMean, color=race))+geom_point()+
  stat_function(fun=equation3,geom="line",color=scales::hue_pal()(2)[1],show.legend = TRUE)+
  stat_function(fun=equation4,geom="line",color=scales::hue_pal()(2)[2],aes(colour="Race: 2"))

install.packages("ggeffects")
library(ggeffects)
library(sjmisc)

ggpredict(raceAndOpen, terms="openNewMean")


#######################################################
# RETRY, with suggested variables
#
# library("psych")
# library("irr")
# library("lavaan")
# library("semPlot")
# library("dominanceanalysis")
# #library("tidyverse")
# #library("caret")
# #library("leaps")
# library("olsrr")
# library("car")
#
#
# data <- read.csv("validandselect.csv", header = TRUE)
# race <- data$race
# neuro <- data.frame(cbind(data$neuro1, data$neuro2, data$neuro3, data$neuro4, data$neuro5, data$neuro6, data$neuro7, data$neuro8, data$neuro9, data$neuro10, data$neuro11, data$neuro12))
# extra <- data.frame(cbind(data$extra1, data$extra2, data$extra3, data$extra4, data$extra5, data$extra6, data$extra7, data$extra8, data$extra9, data$extra10, data$extra11, data$extra12))
# open <- data.frame(cbind(data$open1, data$open2, data$open3, data$open4, data$open5, data$open6, data$open7, data$open8, data$open9, data$open10, data$open11, data$open12))
# agree <- data.frame(cbind(data$agree1, data$agree2, data$agree3, data$agree4, data$agree5, data$agree6, data$agree7, data$agree8, data$agree9, data$agree10, data$agree11, data$agree12))
# consc <- data.frame(cbind(data$consc1, data$consc2, data$consc3, data$consc4, data$consc5, data$consc6, data$consc7, data$consc8, data$consc9, data$consc10, data$consc11, data$consc12))
# cogtest <- data.frame(data$cogtest)
# performance <- data.frame(data$suprate)
#
# colnames(neuro) <- c('neuro1', 'neuro2', 'neuro3', 'neuro4', 'neuro5', 'neuro6', 'neuro7', 'neuro8', 'neuro9', 'neuro10', 'neuro11', 'neuro12')
# colnames(extra) <- c('extra1', 'extra2', 'extra3', 'extra4', 'extra5', 'extra6', 'extra7', 'extra8', 'extra9', 'extra10', 'extra11', 'extra12')
# colnames(open) <- c('open1', 'open2', 'open3', 'open4', 'open5', 'open6', 'open7', 'open8', 'open9', 'open10', 'open11', 'open12')
# colnames(agree) <- c('agree1', 'agree2', 'agree3', 'agree4', 'agree5', 'agree6', 'agree7', 'agree8', 'agree9', 'agree10', 'agree11', 'agree12')
# colnames(consc) <- c('consc1', 'consc2', 'consc3', 'consc4', 'consc5', 'consc6', 'consc7', 'consc8', 'consc9', 'consc10', 'consc11', 'consc12')
# colnames(cogtest) <- 'cogtest'
# colnames(performance) <- 'suprate'
#
# open <- data.frame(cbind(data$open1, data$open3, data$open4, data$open5, data$open7, data$open8, data$open9, data$open10, data$open11, data$open12))
# colnames(open) <- c('open1', 'open3', 'open4', 'open5', 'open7', 'open8', 'open9', 'open10', 'open11', 'open12')
#
# neuroMean <- rowMeans(neuro)
# extraMean <- rowMeans(extra)
# openMean <- rowMeans(open)
# agreeMean <- rowMeans(agree)
# conscMean <- rowMeans(consc)
#
# putTest <- lm(data$suprate ~ data$cogtest + neuroMean + extraMean + openMean + agreeMean + conscMean)
# putTest
# summary(putTest)