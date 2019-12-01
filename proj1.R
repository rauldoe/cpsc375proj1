
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


