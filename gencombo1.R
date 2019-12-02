

# comboNChooseK(cList, n, k)
# comboNChooseKRecursive(cList, n, k, buildList, buildIndex, parentIndex, childIndex, comboList)


cList <- c(1, 2, 3, 4, 5)
n <- length(cList)
k <- 3
buildList <- vector(mode="integer", length=k)
buildIndex <- 0
parentIndex <- 0
childIndex <- 0
comboList <- list()

comboNChooseKRecursive <- function(cList, n, k, buildList, buildIndex, parentIndex, childIndex, comboList) {
  buildIndex = buildIndex + 1
  childIndex = parentIndex + childIndex + 1
  
  if (parentIndex > n-1) {
    return (comboList)
  }
  else if ((buildIndex > k) || (childIndex > n)) {
    cLength = length(comboList) + 1
    comboList[[cLength]] <- buildList
    parentIndex = parentIndex + 1
    
    buildIndex <- 0
    childIndex <- 0
    print(buildList)
    print(paste('call1 ', buildIndex, ' ', parentIndex, ' ', childIndex))
    comboNChooseKRecursive(cList, n, k, buildList, buildIndex, parentIndex, childIndex, comboList)
  }
  else {
    buildList[buildIndex] = cList[childIndex]
    print(buildList)
    print(paste('call2 ', buildIndex, ' ', parentIndex, ' ', childIndex))
    comboNChooseKRecursive(cList, n, k, buildList, buildIndex, parentIndex, childIndex, comboList)
  }
}

x <- comboNChooseKRecursive(cList, n, k, buildList, buildIndex, parentIndex, childIndex, comboList)

#print('combo')
# print(x)
