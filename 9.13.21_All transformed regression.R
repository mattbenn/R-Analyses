# Title     : TODO
# Objective : TODO
# Created by: matth
# Created on: 9/13/2021

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
  hist(myData[,num], main = colnames(myData)[num])
}  # Histograms

for(num in 1:11){
  qqPlot(myData[,num], xlab = "Theorized Quantiles", ylab = "Observed Quantiles", main=colnames(myData)[num])
}  # QQ Plots

# Transformation list:
# 1 Growth.Change.(#): Logarithmic
# 2 Results.Oriented: Logarithmic
# 3 Flexibility: reverse score (101 - score), then Logarithmic
# 4 Emotional.regulation: Logarithmic
# 5 Systematic: reverse score, then Logarithmic
# 6 Competitive: reverse score, then Logarithmic
# 7 Empathy: reverse score, then Logarithmic
# 8 Self.Discipline: reverse score, then Logarithmic
# 9 Perseverance: reverse score, then Logarithmic
# 10 Openness: reverse score, then Logarithmic
# 11 Leadership: reverse score, then Logarithmic

# Column# of reverse scores: 3, 5, 6, 7, 8, 9, 10, 11
# Column# of logarithmic transformation: [all]

logData <- myData # Create log xform data.frame

logData[1] <- logData[1] + 11.5  # Add constant to get minimum value to 1
for(num in c(3, 5, 6, 7, 8, 9, 10, 11)){
  logData[num] <- 101 - logData[num]
}  # Reverse score needed variables

for(num in c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)){
  logData[num] <- log(logData[num], base=10)
}

for(num in 1:11){
  hist(logData[, num], main = colnames(logData)[num], xlab="Transformed data")
  # hist(myData[,num], main = colnames(myData)[num], xlab="Normal data")
}  # Histograms of old and transformed data
allLogDescriptors <- describe(logData)

for(num in 1:11){
  qqPlot(logData[,num], xlab = "Theorized Quantiles", ylab = "Observed Quantiles", main=colnames(logData)[num])
}

View(logData)
write.csv(x=logData, file="RWA data_All log.csv")

################## Regression assumptions
library("ggplot2")
library("broom")
library("GGally")

# Setting up individual regressions
log.results.reg <- lm(`GrowthChange.(#)` ~ Results.Oriented, data=logData)
log.flex.reg <- lm(`GrowthChange.(#)` ~ Flexibility, data=logData)
log.emo.reg <- lm(`GrowthChange.(#)` ~ Emotional.regulation, data=logData)
log.system.reg <- lm(`GrowthChange.(#)` ~ Systematic, data=logData)
log.comp.reg <- lm(`GrowthChange.(#)` ~ Competitive, data=logData)
log.empathy.reg <- lm(`GrowthChange.(#)` ~ Empathy, data=logData)
log.disc.reg <- lm(`GrowthChange.(#)` ~ Self.Discipline, data=logData)
log.pers.reg <- lm(`GrowthChange.(#)` ~ Perserverance, data=logData)
log.open.reg <- lm(`GrowthChange.(#)` ~ Openness, data=logData)
log.leader.reg <- lm(`GrowthChange.(#)` ~ Leadership, data=logData)

log.regList <- list(log.results.reg, log.flex.reg, log.emo.reg, log.system.reg, log.comp.reg, log.empathy.reg, log.disc.reg,
                    log.pers.reg, log.open.reg, log.leader.reg)  # for use with later for loops

# Linearity
for(num in 1:length(log.regList)){
  hist(cooks.distance(log.regList[[num]]), main=log.regList[[num]]$call, xlim= range(0, 1), xlab="Cook's D")
}  # Checking for Cook's D. Anything over 0.5 a problem, 1 == bad. Everything looks good.

# Bivariate normality
for(num in 1:length(log.regList)){
  qqPlot(log.regList[[num]], xlab = "Theorized Quantiles", ylab = "Studentized Residuals", main=log.regList[[num]]$call)
}  # All look acceptable to me. Rows 87 and 25 keep sticking out, but they're the extreme outliers

# Homoscedasticity of residuals
for(num in 1:length(log.regList)){
  spreadLevelPlot(log.regList[[num]], main=log.regList[[num]]$call)
}  # Emotional.Regulation shows some evidence of curvilinearity; I think most others look okay

################ Data fix (only for bivariate regression)
# Based on above, remove rows 25, 53, and 87.
logData <- logData[-c(25, 53, 87),]
# After redoing the above analyses, things look good

################# Zero-order Correlations
library("apaTables")
apa.cor.table(logData, filename = "Correlation Tables_All Log.doc")
# All bivariate correlations insignificant, except for emotional regulation

################# Multiple Regression
log.full.reg <- lm(`GrowthChange.(#)` ~ Results.Oriented + Flexibility + Emotional.regulation + Systematic +
  Competitive + Empathy + Self.Discipline + Perserverance + Openness + Leadership, data=logData) #logGrowth

hist(cooks.distance(log.full.reg), main="All Predictors (-25, 53, 87)", xlim= range(0, 1), xlab="Cook's D") # Linearity. All good.
qqPlot(log.full.reg, xlab = "Theorized Quantiles", ylab = "Studentized Residuals", main="All Predictors (-25, 53, 87")
# Multivariate normality. All good, so long as 25, 53, and 87 are removed
spreadLevelPlot(log.full.reg, main="All Predictors (-25, 53, 87)", id = TRUE)  # id labels certain outliers
# Homoscedasticity of residuals. Some evidence of curvilinearity, I think.

# Multicollinearity
vif(log.full.reg) # VIF
1/vif(log.full.reg) # Tolerance. Highest: .90 (Emotional.regulation) and 0.87 (Competitive); Lowest:.53 (Flexibility)

# Summaries
summary(log.results.reg)
summary(log.flex.reg)
summary(log.emo.reg)
summary(log.system.reg)
summary(log.comp.reg)
summary(log.empathy.reg)
summary(log.disc.reg)
summary(log.pers.reg)
summary(log.open.reg)
summary(log.leader.reg)
summary(log.full.reg)
QuantPsyc::lm.beta(log.full.reg)


################################## RWA analysis (using package rwa)
library(tidyverse)
install.packages("rwa")
library("rwa")

rwaData <- read.csv("RWA data.csv", check.names=FALSE, header=TRUE)

rwaResults <- rwaData %>%
  rwa(outcome = "GrowthChange.(#)",
      predictors = c("Results.Oriented", "Flexibility", "Emotional.regulation", "Systematic",
                     "Competitive", "Empathy", "Self.Discipline", "Perserverance",
                     "Openness", "Leadership"),
      applysigns = TRUE
  )  # This analysis gives the same results as from the rwa shinyapp (https://rwa-web.shinyapps.io/multipleregression/)
# but this analysis doesn't give CIs or other information of statistical significance

rwaData2 <- read.csv("RWA data2.csv", check.names=FALSE, header=TRUE)

rwaResults2 <- rwaData2 %>%
  rwa(outcome = "GrowthChange.(#)",
      predictors = c("Results.Oriented", "Flexibility", "Emotional.regulation", "Systematic",
                     "Competitive", "Empathy", "Self.Discipline", "Perserverance",
                     "Openness", "Leadership"),
      applysigns = TRUE
  )  # Basically the same as above. The shiny app shows that emotional regulation, with a rescaled weight of 70.66,
# is significant, but that's the only one... Same results, albeit with different rescaled weight values and CIs,
# with normal GrowthChange

