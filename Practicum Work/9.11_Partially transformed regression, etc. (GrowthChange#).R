# Title     : TODO
# Objective : TODO
# Created by: matth
# Created on: 9/11/2021

library(psych)

allData <- read.csv("Data.csv", header=TRUE, check.names = FALSE)
colnames(allData) <- c("X1", 'Agent.ID', 'Office.ID', 'Office.Name', 'Office.Address', 'Office.City', 'Office.Zip',
                       'Total.#.(08/01/2020.-.07/31/2021)', 'Total.$.(08/01/2020.-.07/31/2021)', 'SP/OP', 'Avg.DOM',
                       'List.#.(LTM)', 'List.$.(LTM)', 'Sell.#.(LTM)', 'Sell.$.(LTM)', 'Total.#.(LTM)', 'Total.$.(LTM)',
                       'GrowthPTM.#', 'GrowthLTM.#', 'GrowthChange.(#)', 'Growth%.Change.(#)', 'GrowthPTM.$', 'GrowthLTM.$',
                       'GrowthChange.($)', 'Growth%.Change.($)', 'Created.Date', 'Job.Title', 'Group.Name', 'Status',
                       'Match.Score', 'role', 'Results.Oriented', 'Flexibility', 'Emotional.regulation', 'Systematic',
                       'Competitive', 'Empathy', 'Self.Discipline', 'Perserverance', 'Openness', 'Leadership')
allData <- allData[,-c(1,3:7)]  # Remove extraneous columns
allData[allData == -99] <- NA # Replace missing values (-99 in dataset) with NA
descriptors <- psych::describe(allData[c(14,26:35)])
myData <- allData[,c(14,26:35)] # Create working dataset

################################# Regression Assumptions
library(car)
#### Normality
for(num in 1:11){
  hist(myData[,num], main = colnames(myData)[num])
}  # Histograms

for(num in 1:11){
  qqPlot(myData[,num], xlab = "Theorized Quantiles", ylab = "Observed Quantiles", main=colnames(myData)[num])
}  # QQ Plots

for(num in 1:11){
  print(colnames(myData)[num])
  print(shapiro.test(myData[,num]))
}  # Not helpful in deciding which to transform, as all are very significant


# Transformation rules: Only transform those with a skew greater than |1|
# Transformation list:
# 1 Growth.Change.(#): Logarithmic or Winsorized? (UPDATE: Checked both; both return roughly same results)
# 2 Results.Oriented: None
# 3 Flexibility: reverse score (101 - score), then Logarithmic
# 4 Emotional.regulation: Logarithmic
# 5 Systematic: None
# 6 Competitive: reverse score, then Logarithmic
# 7 Empathy: reverse score, then Logarithmic
# 8 Self.Discipline: None
# 9 Perseverance: None
# 10 Openness: None
# 11 Leadership: None

# Column# of reverse scores: 3, 6, 7
# Column# of logarithmic transformation: 1, 3, 4, 6, 7
# Column# of Winsorized: 1

## Winsorized GrowthChange.#
growthQuants <- quantile(myData$`GrowthChange.(#)`, c(.025, .975))
myData$winsorGrowth <- myData$`GrowthChange.(#)` # Column 12
myData$winsorGrowth[myData$winsorGrowth < growthQuants[1]] <- growthQuants[1]
myData$winsorGrowth[myData$winsorGrowth > growthQuants[2]] <- growthQuants[2]

xformData <- myData # Create xform data.frame

xformData[1] <- xformData[1] + 11.5  # Add constant to get minimum value to 1
for(num in c(3, 6, 7)){
  xformData[num] <- 101 - xformData[num]
}  # Reverse score needed variables

for(num in c(1, 3, 4, 6, 7)){
  xformData[num] <- log(xformData[num], base=10)
}

hist(myData$winsorGrowth, main='Winsorized Growth')
for(num in 1:11){
  hist(xformData[,num], main = colnames(xformData)[num], xlab="Transformed data")
  # hist(myData[,num], main = colnames(myData)[num], xlab="Normal data")
}  # Histograms of old and transformed data
xFormDescriptors <- describe(xformData)
# describeAll <- data.frame(descriptors[,11:13], descriptors2[1:11,11:13])  # Comparing distribtion statistics

View(xformData)
write.csv(x=xformData, file="RWA data_Part transform.csv")

################## Regression assumptions
library("ggplot2")
library("broom")
library("GGally")

# Setting up individual regressions
part.results.reg <- lm(`GrowthChange.(#)` ~ Results.Oriented, data=xformData)
part.flex.reg <- lm(`GrowthChange.(#)` ~ Flexibility, data=xformData)
part.emo.reg <- lm(`GrowthChange.(#)` ~ Emotional.regulation, data=xformData)
part.system.reg <- lm(`GrowthChange.(#)` ~ Systematic, data=xformData)
part.comp.reg <- lm(`GrowthChange.(#)` ~ Competitive, data=xformData)
part.empathy.reg <- lm(`GrowthChange.(#)` ~ Empathy, data=xformData)
part.disc.reg <- lm(`GrowthChange.(#)` ~ Self.Discipline, data=xformData)
part.pers.reg <- lm(`GrowthChange.(#)` ~ Perserverance, data=xformData)
part.open.reg <- lm(`GrowthChange.(#)` ~ Openness, data=xformData)
part.leader.reg <- lm(`GrowthChange.(#)` ~ Leadership, data=xformData)

part.regList <- list(part.results.reg, part.flex.reg, part.emo.reg, part.system.reg, part.comp.reg, part.empathy.reg, part.disc.reg,
                     part.pers.reg, part.open.reg, part.leader.reg)  # for use with later for loops

# Linearity
for(num in 1:length(part.regList)){
  hist(cooks.distance(part.regList[[num]]), main=part.regList[[num]]$call, xlim= range(0, 1), xlab="Cook's D")
}  # Checking for Cook's D. Anything over 0.5 a problem, 1 == bad. Everything looks good.

# Bivariate normality
for(num in 1:length(part.regList)){
  qqPlot(part.regList[[num]], xlab = "Theorized Quantiles", ylab = "Studentized Residuals",
         main=part.regList[[num]]$call, id=TRUE)
}  # All look acceptable to me. Rows 25, 53, and 87 keep sticking out, but they're the extreme outliers

# Homoscedasticity of residuals
for(num in 1:length(part.regList)){
  spreadLevelPlot(part.regList[[num]], main=part.regList[[num]]$call)
}  # Emotional.Regulation shows some evidence of curvilinearity; I think most others look okay

################ Redoing everything from above, sans the three problem rows
# Setting up individual regressions
xformData <- xformData[-c(25, 53, 87),]
part.results.reg <- lm(`GrowthChange.(#)` ~ Results.Oriented, data=xformData)
part.flex.reg <- lm(`GrowthChange.(#)` ~ Flexibility, data=xformData)
part.emo.reg <- lm(`GrowthChange.(#)` ~ Emotional.regulation, data=xformData)
part.system.reg <- lm(`GrowthChange.(#)` ~ Systematic, data=xformData)
part.comp.reg <- lm(`GrowthChange.(#)` ~ Competitive, data=xformData)
part.empathy.reg <- lm(`GrowthChange.(#)` ~ Empathy, data=xformData)
part.disc.reg <- lm(`GrowthChange.(#)` ~ Self.Discipline, data=xformData)
part.pers.reg <- lm(`GrowthChange.(#)` ~ Perserverance, data=xformData)
part.open.reg <- lm(`GrowthChange.(#)` ~ Openness, data=xformData)
part.leader.reg <- lm(`GrowthChange.(#)` ~ Leadership, data=xformData)

part.regList <- list(part.results.reg, part.flex.reg, part.emo.reg, part.system.reg, part.comp.reg, part.empathy.reg, part.disc.reg,
                     part.pers.reg, part.open.reg, part.leader.reg)  # for use with later for loops

# Linearity
for(num in 1:length(part.regList)){
  hist(cooks.distance(part.regList[[num]]), main=part.regList[[num]]$call, xlim= range(0, 1), xlab="Cook's D")
}  # Checking for Cook's D. Anything over 0.5 a problem, 1 == bad. Everything looks good.

# Bivariate normality
for(num in 1:length(part.regList)){
  qqPlot(part.regList[[num]], xlab = "Theorized Quantiles", ylab = "Studentized Residuals",
         main=part.regList[[num]]$call, id=TRUE)
}  # All look acceptable to me. Rows 52 and 136 almost violate, but seem to be acceptable

# Homoscedasticity of residuals
for(num in 1:length(part.regList)){
  spreadLevelPlot(part.regList[[num]], main=part.regList[[num]]$call)
}  # Emotional.Regulation shows some evidence of curvilinearity; I think most others look okay

library(QuantPsyc)
lm.beta(part.emo.reg)
lm.beta(part.results.reg)
lm.beta(part.open.reg)
corr.test(xformData$`GrowthChange.(#)`, xformData$Results.Oriented)


################# Zero-order Correlations
library("apaTables")
apa.cor.table(xformData, filename = "Correlation Tables_part xform.doc")
# All bivariate correlations insignificant, except for emotional regulation

################# Multiple Regression
part.full.reg <- lm(`GrowthChange.(#)` ~ Results.Oriented + Flexibility + Emotional.regulation + Systematic +
  Competitive + Empathy + Self.Discipline + Perserverance + Openness + Leadership, data=xformData) #logGrowth

hist(cooks.distance(part.full.reg), main="All Predictors (-25, 53, 87)", xlim= range(0, 1), xlab="Cook's D") # Linearity. All good.
qqPlot(part.full.reg, xlab = "Theorized Quantiles", ylab = "Studentized Residuals", main="All Predictors (-25, 53, 87)")
# Multivariate normality. All good, again except for 25, 53, and 87
spreadLevelPlot(part.full.reg, main="All Predictors (-25, 53, 87)")
# Homoscedasticity of residuals. Very slight funnel effect, likely caused by known outliers

# Multicollinearity
vif(part.full.reg) # VIF
1/vif(part.full.reg) # Tolerance. Highest: .91 (Emotional.regulation); Lowest:.52 (Openness)

# Summaries
summary(part.results.reg)
summary(part.flex.reg)
summary(part.emo.reg)
summary(part.system.reg)
summary(part.comp.reg)
summary(part.empathy.reg)
summary(part.disc.reg)
summary(part.pers.reg)
summary(part.open.reg)
summary(part.leader.reg)
summary(part.full.reg)
QuantPsyc::lm.beta(part.full.reg)

############# Testing winsorGrowth

full.winsor.reg <- lm(winsorGrowth ~ Results.Oriented + Flexibility + Emotional.regulation + Systematic +
  Competitive + Empathy + Self.Discipline + Perserverance + Openness + Leadership, data=xformData)

hist(cooks.distance(full.winsor.reg), main="All Predictors", xlim= range(0,1), xlab="Cook's D") # Linearity. All good.
qqPlot(full.winsor.reg, xlab = "Theorized Quantiles", ylab = "Studentized Residuals", main="All Predictors")
# Multivariate normality. All good, this time with no extreme outliers
spreadLevelPlot(full.winsor.reg, main="All Predictors")
# Homoscedasticity of residuals. A few very small outliers; other than that looks a bit better than logGrowth

# Multicollinearity
vif(full.winsor.reg) # VIF
1/vif(full.winsor.reg) # Tolerance. Highest: .915 (Emotional.regulation); Lowest:.53 (Openness)

summary(full.winsor.reg)


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

