

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
gComboList <- list()

comboNChooseKRecursive <- function(cList, n, k, buildList, buildIndex, start, end, comboList) {
  
  if (buildIndex > k) {

  	cLength <- length(comboList) + 1
  	comboList[[cLength]] <- buildList
  	cLength <- length(gComboList) + 1
  	gComboList[[cLength]] <- buildList
  	
	  print("combo")
	  print(buildList)
	  
  }
  else {

    i <- start
    while ((buildIndex <= k) && (i <= end) ) {   
    	
    	buildList[buildIndex] <- cList[i]
    
    	newI <- i + 1
    	newBuildIndex <- buildIndex + 1
    
    	print(paste("buildIndex:", buildIndex, "i:", i, "end:", end, sep= " "))
      print(buildList)
    	comboNChooseKRecursive(cList, n, k, buildList, newBuildIndex, newI, end, comboList)
    	
    	i <- i + 1
    }
    # while ((buildIndex <= k) && (i <= end) ) {
  }
}

comboNChooseKRecursive(cList, n, k, buildList, buildIndex, start, end, comboList)



print('combo')
print(gComboList)
