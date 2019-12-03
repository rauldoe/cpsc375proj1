
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
  	comboList[[cLength]] <- buildList

  	print(paste("add to comboList"))
  	print(buildList)
  	# return (comboList)	  
  }
  else {
  
    i <- start
    keepGoing <- TRUE
    while (keepGoing) {   
    	
      newI <- i + 1
      newBuildIndex <- buildIndex + 1
      
      buildList[buildIndex] <- cList[i]
    
      # print("newI <- i + 1; newBuildIndex <- buildIndex + 1;")
    	print(paste("recurse buildIndex:", buildIndex, "i:", i, "end:", end, "newBuildIndex:", newBuildIndex, "newI:", newI, sep= " "))
    	print(buildList)
    	comboList <- comboNChooseKRecursive(cList, n, k, buildList, newBuildIndex, newI, end, comboList)
    	print(paste("return from recurse comboList"))
    	
    	i <- i + 1

    	if (i > end) {
    	  print("i > end")
    	  keepGoing <- FALSE
    	  break
    	}
    	else if (n-i+1 < k-buildIndex+1) {
    	  print("n-i+1 < k-buildIndex+1")
    	  keepGoing <- FALSE
    	  break
    	}
    }
    # while ((buildIndex <= k) && (i <= end) ) {
  }
  # if (buildIndex > k) {
  
  return (comboList)
}

comboList <- comboNChooseKRecursive(cList, n, k, buildList, buildIndex, start, end, comboList)

print('combo')
print(comboList)
