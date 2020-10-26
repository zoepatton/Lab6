set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000)
  )

#largest value (sum of v's in the data frame) that is below the overall weight 

#' Largest possible value in weighted bag - brute force
#'
#' This function takes the values and weights provided in a data frame and finds the largest possible sum of values that is less than or equal to the inputed weight by trying all possible combinations. 
#'
#' @param x data frame
#' @param W positive numeric value
#' @param parallel gives opportunity to use parallel programming
#' @return The output will be a numerical value(the largest possible sum) and the corresponding numerical elements that make that sum
#' @import parallel
#' @export brute_force_knapsack


brute_force_knapsack<-function(x,W, parallel=FALSE){
  
  if (!is.data.frame(x)){
    stop("x is not a data frame")
  }
  if (W<0){
    stop("W needs to be positive")
  }
  
  v<-x$v
  w<-x$w
  
  if(all(v > 0) & all(w > 0)){
    
    n<-nrow(x)
    name<-rownames(x)
    
    elementCombos<-list()
    weights<-list()
    values<-list()
    
    if(parallel == FALSE){
      
      for (i in 1:n){
        
        elementCombos[[i]]<-combn(name,i,paste, collapse = " ")
        weights[[i]]<-combn(w,i, FUN=sum)
        values[[i]]<-combn(v,i, FUN=sum)
        
      }
      
    } else if(parallel == TRUE){
      
      cluster <- makeCluster(2)
      registerDoParallel(cluster)
      
      foreach(i = 1:n) %do% {
        
        elementCombos[[i]]<-combn(name,i,paste, collapse = " ")
        weights[[i]]<-combn(w,i, FUN=sum)
        values[[i]]<-combn(v,i, FUN=sum)
        
      }
      stopCluster(cluster)
    } 
    
    maxweight<-which(unlist(weights)<=W)
    correspondingValues<-(round(unlist(values)))[maxweight]
    valuesmax<-max(correspondingValues)
    
    index<-which((round(unlist(values))) == valuesmax)
    coorespondingElement<-(unlist(elementCombos))[index]

    return(list(value=round(valuesmax),elements=as.numeric(unlist(strsplit(coorespondingElement," ")))))
  }
  else
    stop("values are not positive")
}

