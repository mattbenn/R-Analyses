# Title     : TODO
# Objective : TODO
# Created by: matth
# Created on: 4/2/2021

library("psych")
library("irr")
library("lavaan")
library("semPlot")

# Set up data and initial objects
data <- read.csv("validandselect.csv", header = TRUE)
race <- data$race
neuro <- data.frame(cbind(data$neuro1, data$neuro2, data$neuro3, data$neuro4, data$neuro5, data$neuro6, data$neuro7, data$neuro8, data$neuro9, data$neuro10, data$neuro11, data$neuro12))
extra <- data.frame(cbind(data$extra1, data$extra2, data$extra3, data$extra4, data$extra5, data$extra6, data$extra7, data$extra8, data$extra9, data$extra10, data$extra11, data$extra12))
open <- data.frame(cbind(data$open1, data$open2, data$open3, data$open4, data$open5, data$open6, data$open7, data$open8, data$open9, data$open10, data$open11, data$open12))
agree <- data.frame(cbind(data$agree1, data$agree2, data$agree3, data$agree4, data$agree5, data$agree6, data$agree7, data$agree8, data$agree9, data$agree10, data$agree11, data$agree12))
consc <- data.frame(cbind(data$consc1, data$consc2, data$consc3, data$consc4, data$consc5, data$consc6, data$consc7, data$consc8, data$consc9, data$consc10, data$consc11, data$consc12))
extra2 <- data.frame(cbind(data$extra1, data$extra2, data$extra4, data$extra5, data$extra7, data$extra8, data$extra10, data$extra11))
open2 <- data.frame(cbind(data$open3, data$open5, data$open9))
agree2 <- data.frame(cbind(data$agree4, data$agree8))
cogtest <- data.frame(data$cogtest)
performance <- data.frame(data$suprate)
colnames(neuro) <- c('neuro1', 'neuro2', 'neuro3', 'neuro4', 'neuro5', 'neuro6', 'neuro7', 'neuro8', 'neuro9', 'neuro10', 'neuro11', 'neuro12')
colnames(extra) <- c('extra1', 'extra2', 'extra3', 'extra4', 'extra5', 'extra6', 'extra7', 'extra8', 'extra9', 'extra10', 'extra11', 'extra12')
colnames(open) <- c('open1', 'open2', 'open3', 'open4', 'open5', 'open6', 'open7', 'open8', 'open9', 'open10', 'open11', 'open12')
colnames(agree) <- c('agree1', 'agree2', 'agree3', 'agree4', 'agree5', 'agree6', 'agree7', 'agree8', 'agree9', 'agree10', 'agree11', 'agree12')
colnames(consc) <- c('consc1', 'consc2', 'consc3', 'consc4', 'consc5', 'consc6', 'consc7', 'consc8', 'consc9', 'consc10', 'consc11', 'consc12')
colnames(cogtest) <- 'cogtest'
colnames(performance) <- 'suprate'
colnames(extra2) <- c('extra1', 'extra2', 'extra4', 'extra5', 'extra7', 'extra8', 'extra10', 'extra11')
colnames(open2) <- c('open3', 'open5', 'open9')
colnames(agree2) <- c('agree4', 'agree8')
neuro$Mean <- rowMeans(neuro)
consc$Mean <- rowMeans(consc)
extra$Mean <- rowMeans(extra)
open$Mean <- rowMeans(open)
agree$Mean <- rowMeans(agree)
extra2$Mean <- rowMeans(extra2)
open2$Mean <- rowMeans(open2)
agree2$Mean <- rowMeans(agree2)

# Create Extreme Groups
neuroExtreme <- data.frame(quantile(neuro$Mean, c(0.25, 0.75), na.rm=TRUE))
conscExtreme <- data.frame(quantile(consc$Mean, c(0.25, 0.75), na.rm=TRUE))
extraExtreme <- data.frame(quantile(extra$Mean, c(0.25, 0.75), na.rm=TRUE))
openExtreme <- data.frame(quantile(open$Mean, c(0.25, 0.75), na.rm=TRUE))
agreeExtreme <- data.frame(quantile(agree$Mean, c(0.25, 0.75), na.rm=TRUE))
extra2Extreme <- data.frame(quantile(extra2$Mean, c(0.25, 0.75), na.rm=TRUE))
open2Extreme <- data.frame(quantile(open2$Mean, c(0.25, 0.75), na.rm=TRUE))
agree2Extreme <- data.frame(quantile(agree2$Mean, c(0.25, 0.75), na.rm=TRUE))

colnames(neuroExtreme) <- 'Groups'
colnames(conscExtreme) <- 'Groups'
colnames(extraExtreme) <- 'Groups'
colnames(openExtreme) <- 'Groups'
colnames(agreeExtreme) <- 'Groups'
colnames(extra2Extreme) <- 'Groups'
colnames(open2Extreme) <- 'Groups'
colnames(agree2Extreme) <- 'Groups'

# Create Difficulties

# Create Deciles
neuroDec <- data.frame(quantile(neuro$Sum, seq(from=0.1, to=1, by=0.1), na.rm=TRUE))
conscDec <- data.frame(quantile(consc$Sum, seq(from=0.1, to=1, by=0.1), na.rm=TRUE))
extraDec <- data.frame(quantile(extra$Sum, seq(from=0.1, to=1, by=0.1), na.rm=TRUE))
openDec <- data.frame(quantile(open$Sum, seq(from=0.1, to=1, by=0.1), na.rm=TRUE))
agreeDec <- data.frame(quantile(agree$Sum, seq(from=0.1, to=1, by=0.1), na.rm=TRUE))
extra2Dec <- data.frame(quantile(extra2$Sum, seq(from=0.1, to=1, by=0.1), na.rm=TRUE))
open2Dec <- data.frame(quantile(open2$Sum, seq(from=0.1, to=1, by=0.1), na.rm=TRUE))
agree2Dec <- data.frame(quantile(agree2$Sum, seq(from=0.1, to=1, by=0.1), na.rm=TRUE))

colnames(neuroDec) <- 'Deciles'
colnames(conscDec) <- 'Deciles'
colnames(extraDec) <- 'Deciles'
colnames(openDec) <- 'Deciles'
colnames(agreeDec) <- 'Deciles'
colnames(extra2Dec) <- 'Deciles'
colnames(open2Dec) <- 'Deciles'
colnames(agree2Dec) <- 'Deciles'