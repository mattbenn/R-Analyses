# Title     : Echovate data: Merge and Clean
# Objective : Merge and clean performance and predictive data for Echovate project
# Created by: matth
# Created on: 9/3/2021

##### Put both datafiles, as downloaded from Blackboard on 9/3/21, in the same folder
library(openxlsx)  # You need this package to run one of the functions

# Import data files
perfData <- openxlsx::read.xlsx("Data.xlsx",
                                sheet=1)
predData <- read.csv("Other Data.csv", header=TRUE)

# Clean files
predData <- predData[complete.cases(predData),]  # Remove those without Agent IDs
predData <- predData[order(predData$Agent.ID),]  # Order dataframe by Agent ID
rownames(predData) <- 1:nrow(predData)  # Reset dataframe row index


predData$Duplicates <- 0  # Create empty row to populate below
freqTable <- table(predData$Agent.ID) # Create frequency table to calculate number of times Agent.ID appears
for(number in 1:nrow(predData)){
  predData$Duplicates[number] <- freqTable[names(freqTable)==predData$Agent.ID[number]]
}  # Add row showing number of duplicates, for sorting data frame by them

#### Remove extra data from predData
# Rules:
# 1) When more than one observation with same Agent ID, keep those with GroupName 'Agents'
# 2) If still multiples, keep highest MatchScore
# 3) If still multiples, pick at random

# Visually scanned predData to determine the index numbers of rows to delete
# (See 'Removed items.xlsx' for list of reasons why)

predDeleted <- predData[c(6, 13, 18, 21, 37, 44, 68, 76, 90, 108, 114, 127, 135, 139, 143),]
predData <- predData[-c(6, 13, 18, 21, 37, 44, 68, 76, 90, 108, 114, 127, 135, 139, 143),]
predData <- predData[-ncol(predData)]  # Remove Duplicates column
nrow(predData)  # Final version of predData has 139 observations


##### Clean perfData and prepare for join
perfData <- perfData[order(perfData$Agent.ID),]
rownames(perfData) <- 1:nrow(perfData)
dataMerge <- merge(x=perfData, y=predData, by="Agent.ID", all.x=TRUE)  # merge data

write.csv(x=dataMerge, file="Merged Data.csv", na="-99")  # Create final .csv file