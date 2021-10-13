# Title     : TODO
# Objective : TODO
# Created by: matth
# Created on: 9/12/2021

library(psych)

allData <- read.csv("Merged Echovate Data.csv", header=TRUE, check.names = FALSE)
colnames(allData) <- c("X1", 'Agent.ID', 'Office.ID', 'Office.Name', 'Office.Address', 'Office.City', 'Office.Zip',
                       'Total.#.(08/01/2020.-.07/31/2021)', 'Total.$.(08/01/2020.-.07/31/2021)', 'SP/OP', 'Avg.DOM',
                       'List.#.(LTM)', 'List.$.(LTM)', 'Sell.#.(LTM)', 'Sell.$.(LTM)', 'Total.#.(LTM)', 'Total.$.(LTM)',
                       'GrowthPTM.#', 'GrowthLTM.#', 'GrowthChange.(#)', 'Growth%.Change.(#)', 'GrowthPTM.$', 'GrowthLTM.$',
                       'GrowthChange.($)', 'Growth%.Change.($)', 'Created.Date', 'Job.Title', 'Group.Name', 'Status',
                       'Match.Score', 'role', 'Results.Oriented', 'Flexibility', 'Emotional.regulation', 'Systematic',
                       'Competitive', 'Empathy', 'Self.Discipline', 'Perserverance', 'Openness', 'Leadership')
allData <- allData[,-c(1,3:7)]  # Remove extraneous columns
allData[allData == -99] <- NA
descriptors <- psych::describe(allData[c(14,26:35)])
myData <- allData[,c(14,26:35)]

################################# Regression Assumptions
library(car)
#### Normality
for(num in 1:11){
  hist(myData[,num], main = colnames(myData)[num], xlab="Value")
}  # Histograms

for(num in 1:11){
  qqPlot(myData[,num], xlab = "Theorized Quantiles", ylab = "Observed Quantiles", main=colnames(myData)[num])
}  # QQ Plots


View(myData)
write.csv(x=myData, file="Raw RWA data.csv")

################## Regression assumptions
library("ggplot2")
library("broom")
library("GGally")

# Setting up individual regressions
results.reg <- lm(`GrowthChange.(#)` ~ Results.Oriented, data=myData)
flex.reg <- lm(`GrowthChange.(#)` ~ Flexibility, data=myData)
emo.reg <- lm(`GrowthChange.(#)` ~ Emotional.regulation, data=myData)
system.reg <- lm(`GrowthChange.(#)` ~ Systematic, data=myData)
comp.reg <- lm(`GrowthChange.(#)` ~ Competitive, data=myData)
empathy.reg <- lm(`GrowthChange.(#)` ~ Empathy, data=myData)
disc.reg <- lm(`GrowthChange.(#)` ~ Self.Discipline, data=myData)
pers.reg <- lm(`GrowthChange.(#)` ~ Perserverance, data=myData)
open.reg <- lm(`GrowthChange.(#)` ~ Openness, data=myData)
leader.reg <- lm(`GrowthChange.(#)` ~ Leadership, data=myData)

regList <- list(results.reg, flex.reg, emo.reg, system.reg, comp.reg, empathy.reg, disc.reg,
                pers.reg, open.reg, leader.reg)  # for use with later for loops

# Linearity
for(num in 1:length(regList)){
  hist(cooks.distance(regList[[num]]), main=regList[[num]]$call, xlim= range(0,1), xlab="Cook's D")
}  # Checking for Cook's D. Anything over 0.5 a problem, 1 == bad. Empathy is an issue.
range(cooks.distance(empathy.reg))  # Finding highest value
cooks.distance(empathy.reg)[cooks.distance(empathy.reg)>0.5]  # Shows that 52 and 53 are issues, at over 0.5.

# Bivariate normality
for(num in 1:length(regList)){
  qqPlot(regList[[num]], xlab = "Theorized Quantiles", ylab = "Studentized Residuals", main=regList[[num]]$call)
}  # Observations 52 and 136 (the extreme outliers for GrowthChange) keep violating assumptions of normality.

# Decided to trim, because there isn't an attendant phenomenon in the negative tail.
myData <- myData[-c(52, 136),]  # Remove outliers

######################### Rerunning regression, etc. with trimmed data
# Setting up individual regressions
results.reg <- lm(`GrowthChange.(#)` ~ Results.Oriented, data=myData)
flex.reg <- lm(`GrowthChange.(#)` ~ Flexibility, data=myData)
emo.reg <- lm(`GrowthChange.(#)` ~ Emotional.regulation, data=myData)
system.reg <- lm(`GrowthChange.(#)` ~ Systematic, data=myData)
comp.reg <- lm(`GrowthChange.(#)` ~ Competitive, data=myData)
empathy.reg <- lm(`GrowthChange.(#)` ~ Empathy, data=myData)
disc.reg <- lm(`GrowthChange.(#)` ~ Self.Discipline, data=myData)
pers.reg <- lm(`GrowthChange.(#)` ~ Perserverance, data=myData)
open.reg <- lm(`GrowthChange.(#)` ~ Openness, data=myData)
leader.reg <- lm(`GrowthChange.(#)` ~ Leadership, data=myData)

regList <- list(results.reg, flex.reg, emo.reg, system.reg, comp.reg, empathy.reg, disc.reg,
                pers.reg, open.reg, leader.reg)  # for use with later for loops

# Linearity
for(num in 1:length(regList)){
  hist(cooks.distance(regList[[num]]), main=regList[[num]]$call, xlim= range(0,1), xlab="Cook's D")
}  # After trimming, everything looks much better. Still an issue with empahy, explored below.
hist(cooks.distance(empathy.reg), main="Emotional Regulation", xlab="Cook's D", ylim=range(0,5))
cooks.distance(empathy.reg)[cooks.distance(empathy.reg)>0.5]  # Problem observation: #53, D = 0.67
# Decided that one observation of 0.6 not a problem; continuing

# Bivariate normality
for(num in 1:length(regList)){
  qqPlot(regList[[num]], xlab = "Theorized Quantiles", ylab = "Studentized Residuals", main=regList[[num]]$call)
}  # I think everything looks acceptable here

# Homoscedasticity of residuals
for(num in 1:length(regList)){
  spreadLevelPlot(regList[[num]], main=regList[[num]]$call)
}  # Slight funnel effect for Perserverance, Competitive, Systematic. Not sure if these are issues for the final
# multiple regression

################# Zero-order Correlations
library("apaTables")
apa.cor.table(myData, filename = "Correlation Tables_no transformations.doc")
# Many bivariate correlations significant... Very different than from untransformed data

################# Multiple Regression
full.reg <- lm(`GrowthChange.(#)` ~ Results.Oriented + Flexibility + Emotional.regulation + Systematic +
  Competitive + Empathy + Self.Discipline + Perserverance + Openness + Leadership, data=myData) #logGrowth
summary(full.reg)

hist(cooks.distance(full.reg), main="All Predictors", xlim= range(0,1), xlab="Cook's D") # Linearity. All good.
qqPlot(full.reg, xlab = "Theorized Quantiles", ylab = "Studentized Residuals", main="All Predictors") # Multivariate normality. All good, again
spreadLevelPlot(full.reg, main="All Predictors", id=TRUE) # Homoscedasticity of residuals. Very slight funnel effect, caused primarily by 179

# Multicollinearity
vif(full.reg) # VIF
1/vif(full.reg) # Tolerance. Highest: .94 (Emotional.regulation); Lowest:.45 (Flexibility)

write.csv(myData, file="RWA data_no xform.csv")

# Testing to see how the two (almost) significant predictors, Emo.reg and Results.oriented, behave on their own
part.reg <- lm(`GrowthChange.(#)` ~ Results.Oriented + Emotional.regulation, data=myData)
summary(part.reg)  # Similar to multiple regression before
hist(cooks.distance(part.reg), main="All Predictors", xlim= range(0,1), xlab="Cook's D") # Linearity. All good.
qqPlot(part.reg, xlab = "Theorized Quantiles", ylab = "Studentized Residuals", main="All Predictors") # Multivariate normality. All good, again
spreadLevelPlot(part.reg, main="All Predictors", id=TRUE)
1/vif(part.reg) # 0.994 for both. They have almost no overlap, as evidenced by low bivariate correlation (.07)

# Summaries
summary(results.reg)
summary(flex.reg)
summary(emo.reg)
summary(system.reg)
summary(comp.reg)
summary(empathy.reg)
summary(disc.reg)
summary(pers.reg)
summary(open.reg)
summary(leader.reg)
summary(full.reg)
QuantPsyc::lm.beta(full.reg)

################################## Dominance analysis (using package rwa)
library("dominanceanalysis")

da <- dominanceAnalysis(full.reg)
summary(da)
write.csv(summary(da), file="dom analysis.csv")

