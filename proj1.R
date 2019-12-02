
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("class")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(class)

# TODO: to be filled
bodyfatpercentage <- function(Age, Weight, Height, Neck, Chest, Abdomen, Hip, Thigh, Knee, Ankle, Biceps, Forearm, Wrist) {
  # your code goes here
}



data <- read.csv("http://staff.pubhealth.ku.dk/~tag/Teaching/share/data/Bodyfat.csv")
# data <- read.csv("C:/temp/cpsc375proj1/Bodyfat.csv")

data <- na.omit(data)

# Plotting
ggplotFunc <- function(aesFunc, labelX, labelY, title) {
  ggplot(data, aesFunc()) + geom_point() + labs(x = labelX, y = labelY, title = title) + geom_smooth(method = 'lm', se = FALSE)
} 

labelY <- "Abdomen Size"
aesFunc <- function() aes(x=bodyfat, y=Abdomen)
labelX <- "Body Fat"
title <- paste("Scatterplot of ", labelY, " on ", labelX)
ggplotFunc(aesFunc = aesFunc, labelX, labelY, title)

labelY <- "Weight"
aesFunc <- function() aes(x=bodyfat, y=Weight)
labelX <- "Body Fat"
title <- paste("Scatterplot of ", labelY, " on ", labelX)
ggplotFunc(aesFunc = aesFunc, labelX, labelY, title)

labelY <- "BMI"
aesFunc <- function() aes(x=bodyfat, y=BMI)
labelX <- "Body Fat"
title <- paste("Scatterplot of ", labelY, " on ", labelX)
ggplotFunc(aesFunc = aesFunc, labelX, labelY, title)

labelY <- "Hip"
aesFunc <- function() aes(x=bodyfat, y=Hip)
labelX <- "Body Fat"
title <- paste("Scatterplot of ", labelY, " on ", labelX)
ggplotFunc(aesFunc = aesFunc, labelX, labelY, title)

labelY <- "Chest"
aesFunc <- function() aes(x=bodyfat, y=Chest)
labelX <- "Body Fat"
title <- paste("Scatterplot of ", labelY, " on ", labelX)
ggplotFunc(aesFunc = aesFunc, labelX, labelY, title)

labelY <- "Thigh"
aesFunc <- function() aes(x=bodyfat, y=Thigh)
labelX <- "Body Fat"
title <- paste("Scatterplot of ", labelY, " on ", labelX)
ggplotFunc(aesFunc = aesFunc, labelX, labelY, title)


labelY <- "Age"
aesFunc <- function() aes(x=bodyfat, y=Age)
labelX <- "Body Fat"
title <- paste("Scatterplot of ", labelY, " on ", labelX)
ggplotFunc(aesFunc = aesFunc, labelX, labelY, title)


labelY <- "Density"
aesFunc <- function() aes(x=bodyfat, y=Density)
labelX <- "Body Fat"
title <- paste("Scatterplot of ", labelY, " on ", labelX)
ggplotFunc(aesFunc = aesFunc, labelX, labelY, title)

# Formulation

# bfFormula <- bodyfat~Hip+Thigh
# lmData <- lm(data = data, formula = bfFormula)
# sum <- summary(lmData)
# sum$r.squared
# sum$adj.r.squared

buildExploreList <- function(exploreList, bfFormula) {
  lmData <- lm(data = data, formula = bfFormula)
  sum <- summary(lmData)
  nextIndex <- length(exploreList) + 1
  exploreList[[nextIndex]] <- list(bfFormula, lmData, sum)
  
  return (exploreList)
}

exploreList <- list()

exploreList <- buildExploreList(exploreList, bodyfat~Chest)
exploreList <- buildExploreList(exploreList, bodyfat~Abdomen)
exploreList <- buildExploreList(exploreList, bodyfat~Thigh)
exploreList <- buildExploreList(exploreList, bodyfat~Chest)
exploreList <- buildExploreList(exploreList, bodyfat~Thigh+Chest)
exploreList <- buildExploreList(exploreList, bodyfat~Thigh+Abdomen)
exploreList <- buildExploreList(exploreList, bodyfat~Chest+Abdomen)
exploreList <- buildExploreList(exploreList, bodyfat~Chest+Abdomen+Thigh)
exploreList <- buildExploreList(exploreList, bodyfat~log(Chest)+log(Abdomen)+log(Thigh))
exploreList <- buildExploreList(exploreList, bodyfat~Chest^2+Abdomen^2+Thigh^2)

for(i in exploreList){print(i[[3]]["r.squared"])}
for(i in exploreList){print(i[[3]]["adj.r.squared"])}
