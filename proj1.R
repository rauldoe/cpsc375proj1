
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

# Combo Generation
comboNChooseKRecursive <- function(inputList, n, k, buildList, buildIndex, start, end, comboList) {
  
  if (buildIndex > k) {
    
    cLength <- length(comboList) + 1
    comboList[[cLength]] <- buildList
    # print(paste("add to comboList", paste(buildList, collapse=","), sep = " "))
  }
  else {
    
    i <- start
    while ((i <= end) &&  (n-i+1 >= k-buildIndex+1)) {   
      
      buildList[buildIndex] <- inputList[i]
      
      newIndex <- i + 1
      newBuildIndex <- buildIndex + 1
      
      # print(paste("recurse buildIndex:", buildIndex, "i:", i, "end:", end, "newBuildIndex:", newBuildIndex, "newIndex:", newIndex, "b:", paste(buildList, collapse=","), sep= " "))
      comboList <- comboNChooseKRecursive(inputList, n, k, buildList, newBuildIndex, newIndex, end, comboList)
      # print(paste("return from recurse comboList buildIndex:", buildIndex, "i:", i, "end:", end, "newBuildIndex:", newBuildIndex, "newIndex:", newIndex, sep= " "))
      
      i <- i + 1
      
    }
    # while ((i <= end) &&  (n-i+1 >= k-buildIndex+1)) {
  }
  # if (buildIndex > k) {
  
  return (comboList)
}

comboNChooseK <- function(inputList, k) {
  n <- length(inputList)
  buildList <- vector(length = k)
  buildIndex1 <- 1
  start <- 1
  end <- n
  comboList <- list()
  comboList <- comboNChooseKRecursive(inputList, n, k, buildList, buildIndex1, start, end, comboList)
  
  return (comboList)
}

comboNChooseKAll <- function(inputList, k) {
  
  comboListAll <- list()
  iLength <- length(inputList)
  
  if (k <= 0) {
    k = iLength
  }
  
  i <- 1
  while (i<=k) {   
    
    comboList <- comboNChooseK(inputList, i)
    comboListAll <- do.call(c, list(comboListAll, comboList))
    i <- i + 1
  }
  
  return (comboListAll)
}

# Function Generation
genFuncCall <- function(inputList, funcCall) {
  return (lapply(1:3, function(i) paste(funcCall, "(", i, ")", sep = '', collapse = ' ')))
}

genPostscriptOpCall <- function(inputList, postscriptOp) {
  return (lapply(1:3, function(i) paste(i, postscriptOp, sep = '', collapse = ' ')))
}

# Plotting
ggplotFunc <- function(aesFunc, labelX, labelY, title) {
  ggplot(data, aesFunc()) + geom_point() + labs(x = labelX, y = labelY, title = title) + geom_smooth(method = 'lm', se = FALSE)
} 

# Execute linear modeling

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


# Execution
data <- read.csv("http://staff.pubhealth.ku.dk/~tag/Teaching/share/data/Bodyfat.csv")
# data <- read.csv("C:/temp/cpsc375proj1/Bodyfat.csv")

data <- na.omit(data)

# Plotting
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

# iVars <- getIndependentVars(cols, dependentAttrib, excludes)
getIndependentVars <- function(cols, dependentAttrib, excludes) {
  
  iVector <- cols[!cols == dependentAttrib]
  for (ex in excludes) {
    iVector <- iVector[!iVector == ex]
  }
  
  return (iVector)
}

# bf <- getBfFormula('bodyfat', c('Thigh', 'knee'))
getBfFormula <- function(dependentAttrib, independentVars) {
  iVars <- paste(independentVars, collapse = "+")
  return (as.formula(paste(dependentAttrib, "~", iVars)))
}

genFuncCall <- function(inputList, funcCall) {
  return (lapply(inputList, function(i) paste(funcCall, "(", i, ")", sep = '', collapse = ' ')))
}

genPostscriptOpCall <- function(inputList, postscriptOp) {
  return (lapply(inputList, function(i) paste(i, postscriptOp, sep = '', collapse = ' ')))
}

buildInFuncs <- function(allIVars, iVars, funcNameList){
  
  for (funcName in funcNameList) {
    funcList <- genFuncCall(as.list(iVars), funcName)
    for (func in funcList) {
      allIVars[[length(allIVars)+1]] <- func
    }
  }
  
  return (allIVars)
}

buildInPostscriptOps <- function(allIVars, iVars, postscriptOpNameList){
  
  for (postscriptOpName in postscriptOpNameList) {
    postscriptList <- genPostscriptOpCall(as.list(iVars), postscriptOpName)
    for (postscript in postscriptList) {
      allIVars[[length(allIVars)+1]] <- postscript
    }
  }
  
  return (allIVars)
}

compileExploreListWithComboList <- function(exploreList, comboList, dependentAttrib) {
  for (combo in comboList) {
    
    bf <- getBfFormula(dependentAttrib, combo)
    exploreList <- buildExploreList(exploreList, bf)
  }
  
  return (exploreList)
}

calculateBestfit <- function(exploreList, maxItemsCount) {
  
  maxValueList <-vector(mode="numeric", length=maxItemsCount)

  maxItemList <- list()
  maxItemList[[1]] <- exploreList[[1]]
  maxValueList[1] <- exploreList[[1]][[3]]["adj.r.squared"]
  
  for (i in 2:maxItemCount) {
    maxItemList[[length(maxItemList)+1]] <- exploreList[[1]]
    maxValueList[i] <- exploreList[[1]][[3]]["adj.r.squared"]
  }
  
  for(ex in exploreList) {
    rsquared = as.numeric(ex[[3]]["r.squared"])
    rsquaredadj = as.numeric(ex[[3]]["adj.r.squared"])
    # print(i[[3]]["adj.r.squared"])
    
    for (index in 1:length(maxValueList))
    {
      if (rsquaredadj > maxValueList[index]) {
        form <- paste(ex[[1]], collapse = " ")
        print(paste("max", form, rsquaredadj))
        maxValueList[index] <- rsquaredadj
        
        maxItemList[[index]] <- ex
        break
      }
    }
  }
  
  return (maxItemList)
}

parseFormulaRecursive <- function (ideList, indepLanguage) {
  separator <- '+'
  for (i in 1:length(indepLanguage)) {
    token <- indepLanguage[[i]]
    type <- typeof(token)
    
    if (type == 'symbol') {
      if (token != separator) {
        ideList[[length(ideList)+1]] <- token
      }
    }
    else {
      
      if (!grepl(separator, toString(token), fixed = TRUE)) {
        ideList[[length(ideList)+1]] <- token
      }
      else {
        ideList <- parseFormulaRecursive(ideList, token)
      }
    }
  }
  
  return (ideList)
}

parseFormula <- function(indepLanguage) {
  ideList <- list()
  ideList <- parseFormulaRecursive(ideList, indepLanguage)
  
  return (ideList)
}

getEquation <- function(item) {
  
  bfFormula <- item[[1]]
  
  # '~'
  # bfFormula[[1]] 
  
  # 'bodyfat'
  dependent <- bfFormula[[2]]
  
  # log(sin(a))+b+c
  indep <- bfFormula[[3]]
  
  iList <- parseFormula(indep)
  # print(iList)
  bfSummary <- item[[2]]
  coff <- bfSummary$coefficients
  intercept <- coff["(Intercept)"]
  coffValueList <- coff[2:length(coff)]
  # print(coffValueList)
  
  n <- names(coffValueList)
  coffEqnList <- vector(length=length(coffValueList))
  for (i in 1:length(coffValueList)) {
    coffEqnList[i] <- paste(coffValueList[i], "*", n[i])
  }
  
  coffEqn = paste(coffEqnList, collapse = "+")
  
  equation <- paste(dependent, "=", intercept, "+", coffEqn)
  
  return (equation)
}

generateBestFitEquations <- function(bestFitList) {
  eqnList <- vector(mode="character", length = length(bestFitList))
  
  for (i in 1:length(bestFitList)) {
    eqn <- getEquation(bestFitList[[i]])
    eqnList[i] <- eqn
  }
  
  return (eqnList)
}

dependentAttrib = "bodyfat"
excludes = c("Density")

# k <- 3
# funcNameList <- c("log", "exp", "sqrt", "log10", "floor", "ceiling", "sin", "cos", "tan")
# postscriptOpNameList <- c("^2", "^3")

k <- 2
funcNameList <- c("log", "sin")
postscriptOpNameList <- c("^2")
maxItemCount <- 3

exploreList <- list()
cols = colnames(data)
iVars <- getIndependentVars(cols, dependentAttrib, excludes)
allIVars <-iVars

allIVars <- buildInFuncs(allIVars, iVars, funcNameList)
allIVars <- buildInPostscriptOps(allIVars, iVars, postscriptOpNameList)

comboList <- comboNChooseKAll(as.list(allIVars), k)

exploreList <- compileExploreListWithComboList(exploreList, comboList, dependentAttrib)

bestFitList <- calculateBestfit(exploreList, maxItemCount)

for (i in bestFitList) {
  
  print(i[[3]]["adj.r.squared"])
}

generateBestFitEquations(bestFitList)



