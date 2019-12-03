
# comboNChooseK(cList, n, k)
# comboNChooseKRecursive(cList, n, k, buildList, buildIndex, start, end, comboList)


cList <- c(1, 2, 3, 4, 5)
n <- length(cList)
k <- 3
buildList <- vector(mode="integer", length=k)
buildIndex <- 1
start <- 1
end <- n
comboList <- list()

comboNChooseKRecursive <- function(cList, n, k, buildList, buildIndex, start, end, comboList) {

  if (buildIndex > k) {

  	cLength <- length(comboList) + 1
  	comboList[[cLength]] <- buildList

  	print(paste("add to comboList", paste(buildList, collapse=","), sep = " "))
  	# return (comboList)	  
  }
  else {
  
    i <- start
    keepGoing <- TRUE
    while (keepGoing) {   
    	
      if (i > end) {
        print(paste("i > end", "i", i, sep =" "))
        keepGoing <- FALSE
        break
      }
      else if (n-i+1 < k-buildIndex+1) {
        print(paste("n-i+1 < k-buildIndex+1", "i", i, "buildIndex", buildIndex, sep = " "))
        keepGoing <- FALSE
        break
      }
      
      buildList[buildIndex] <- cList[i]
      
      newI <- i + 1
      newBuildIndex <- buildIndex + 1
    
    	print(paste("recurse buildIndex:", buildIndex, "i:", i, "end:", end, "newBuildIndex:", newBuildIndex, "newI:", newI, "b:", paste(buildList, collapse=","), sep= " "))
    	comboList <- comboNChooseKRecursive(cList, n, k, buildList, newBuildIndex, newI, end, comboList)
    	print(paste("return from recurse comboList buildIndex:", buildIndex, "i:", i, "end:", end, "newBuildIndex:", newBuildIndex, "newI:", newI, sep= " "))
    	
    	i <- i + 1

    }
    # while ((buildIndex <= k) && (i <= end) ) {
  }
  # if (buildIndex > k) {
  
  return (comboList)
}

comboList <- comboNChooseKRecursive(cList, n, k, buildList, buildIndex, start, end, comboList)

print('combo')
print(comboList)
