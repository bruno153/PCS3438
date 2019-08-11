EP1_USPnumber <- function(fIn, fOut){
  #Get start time
  startTime <- Sys.time()
  #Import library
  library(gtools)
  library(liqueueR)
  library(optrees)
  
  #Get table
  mDist <- as.matrix(read.table(fIn, sep = ",", header = TRUE)) #With header
  #mDist <- as.matrix(read.table(fIn, sep = "", header = FALSE))  #Without header
  
  #Initializes variables and priority queue
  iN <- ncol(mDist)
  iBest <- Inf
  queue <- PriorityQueue$new()
  queue$push(c(1), 0)
  mCities <- seq(1, iN, 1)
  
  #Cost calculating function
  Cost <- function(mSolution){
    iCost <- 0
    for (i in seq(1, length(mSolution) - 1, 1)){
      iCost <- iCost + mDist[mSolution[i], mSolution[i+1]]
    }
    return(iCost)
  }
  
  #Heuristics calculating function
  Heuristics <- function(mSolution){
    #mRest lists all not visited cities
    mRest <- setdiff(mCities, mSolution)
    if (length(mRest) == 0){
      return(0)
    }
    
    #Removes all visited cities distances from distance matrix
    mRestDist <- mDist[,mRest]
    #Gets the shortest way leading and coming from non visited cities
    iInDist <- min(mDist[tail(mSolution, n = 1),])
    iOutDist <- min(mDist[1,])
    iCost <- iInDist + iOutDist
    
    #If there is only one non visited city, ends calculation
    if (length(mRest) < 2){
      return(iCost)
    }
    
    mEdges <- c()
    
    #Build subgraph to search for minimum spanning tree
    for (i in 1:(length(mRest)-1)){
      for (j in (i+1):length(mRest)){
        if (mDist[mRest[i], mRest[j]] < Inf){
          mEdges <- c(mEdges, mRest[i], mRest[j], mDist[mRest[i], mRest[j]])
        }
      }
    }
    
    #If there's edges on subgraph, there's no MST, the calculation thus ends
    if (length(mEdges) == 0){
      return(Inf)
    }
    
    #Reshape subgraph data
    mEdges <- matrix(mEdges, ncol = 3, byrow = TRUE)
    #Verify if the graph is connected
    lComponents <- getComponents(mRest, mEdges)$component
    #Runs Kruskal if the graph is connected, ends process otherwise
    if (length(lComponents) == 1 && length(lComponents[[1]]) == length(mRest)){
      iCost <- sum(msTreeKruskal(mRest, mEdges)$tree.arcs[,3])
    }
    else{
      iCost <- Inf
    }
    return(iCost)
  }
  
  #Begin BFS search
  while (queue$size() > 0){
    mSolution <- queue$pop()
    #If the cycle is not complete yet
    if (length(mSolution) < iN + 1){
      mRest <- setdiff(mCities, mSolution)
      #If the cycle is still incomplete
      if (length(mRest) > 0){
        for (i in mRest){
          queue$push(c(mSolution, i), -(Cost(c(mSolution, i)) + Heuristics(c(mSolution, i))))
        }
      }
      #If the cycle is complete, adds the 1 to close it
      else{
        queue$push(c(mSolution, 1), -Cost(c(mSolution, 1)))
      }
      
    }
    #If the cycle is complete and closed
    else{
      iBest <- Cost(mSolution)
      mBest <- mSolution
      break
    }
  }
  
  
  endTime <- Sys.time()
  #Removes the repeated 1 from answer
  mBest <- head(mBest, -1)
  #Builds the answer vector in a single row vector
  mAns <- t(c(iBest, (endTime - startTime), mBest))
  #Prints all info
  cat("Custo: ", iBest, "\n")
  cat("Tempo de execucao: ", ((endTime - startTime)), "\n")
  cat("Rota: ", mBest, "\n")
  #Writes on output file
  write.table(mAns, file = fOut, sep = ",", row.names = FALSE, col.names = FALSE)
  
}