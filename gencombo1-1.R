

# comboNChooseK(cList, n, k)
# comboNChooseKRecursive(cList, n, k, buildList, buildIndex, start, end, comboList)


cList <- c(1, 2, 3, 4, 5)
n <- length(cList)
k <- 3
buildList <- vector(mode="integer", length=k)
buildIndex <- 0
start <- 0
end <- n
comboList <- list()

comboNChooseKRecursive <- function(cList, n, k, buildList, buildIndex, start, end, comboList) {
  
  if (buildIndex > k) {

  	cLength <- length(comboList) + 1
  	# print(paste("before", length(gComboList)))
  	comboList[[cLength]] <- buildList
  	# print(paste("after", length(gComboList)))

  	print("build")
  	print(buildList)
  	return (comboList)	  
  }
  else {

    i <- start
    while ((i <= end) && (end - buildIndex <= buildIndex - i)) {   
    	
    	buildList[buildIndex] <- cList[i]
    
    	newI <- i + 1
    	newBuildIndex <- buildIndex + 1
    
    	print(paste("buildIndex:", buildIndex, "i:", i, "end:", end, sep= " "))
    	print(buildList)
    	comboList <- comboNChooseKRecursive(cList, n, k, buildList, newBuildIndex, newI, end, comboList)
    	# print("main")
    	# print(comboList)
    	
    	i <- i + 1
    	
    	if (i>end)
    	  break
    }
    # while ((buildIndex <= k) && (i <= end) ) {
  }
  # if (buildIndex > k) {
  
  return (comboList)
}

comboList <- comboNChooseKRecursive(cList, n, k, buildList, buildIndex, start, end, comboList)



print('combo')

print(comboList)
