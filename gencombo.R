
dlist <- c('a', 'b', 'c', 'd', 'e')

buildListRecursive <- function(dataList, buildList, i, comboLength, comboIndex) {
  if ((comboLength - comboIndex) < 0) {
    return (buildList)
  }
  else {
    j = i + 1
    comboIndex = comboIndex + 1
    buildList <- append(buildList, dataList[i])
    return (buildListRecursive(dataList, buildList, j, comboLength, comboIndex))
  }
}

generateCombo <- function(dataList) {
  comboList <- c()
  dLength <- length(dataList)
  
  for (comboLength in 1:dLength) {
    for (i in 1:dLength) {
      comboIndex = 1
      buidList <- c()
      comboItem <- paste(buildListRecursive(dataList, buidList, i, comboLength, comboIndex), collapse = '')
      print(paste('combo: ', comboItem))
      comboList <- append(comboList, comboItem)
    }
  }
  
  return (comboList)
}

res <- generateCombo(dlist)


