
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

comboNChooseKAll <- function(inputList) {
  
  comboListAll <- list()
  iLength <- length(inputList)
  
  i <- 1
  while (i<=iLength) {   
  
    comboList <- comboNChooseK(inputList, i)
    comboListAll <- do.call(c, list(comboListAll, comboList))
    i <- i + 1
  }
  
  return (comboListAll)
}

genFuncCall <- function(inputList, funcCall) {
  return (lapply(1:3, function(i) paste(funcCall, "(", i, ")", sep = '', collapse = ' ')))
}

genPostscriptOpCall <- function(inputList, postscriptOp) {
  return (lapply(1:3, function(i) paste(i, postscriptOp, sep = '', collapse = ' ')))
}

inList <- c("1", "2", "3")

genFuncCall(inList, "log")
genPostscriptOpCall(inList, "^2")


# comboLista <- comboNChooseKAll(inList)

# print(comboLista)
