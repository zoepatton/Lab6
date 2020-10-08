#' Largest possible value in weighted bag - brute force
#'
#' This function takes the values and weights provided in a data frame and finds the largest possible sum of values that is less than or equal to the inputed weight by trying all possible combinations. 
#'
#' @param x data frame
#' @param W positive numeric value
#' @return The output will be a numerical value(the largest possible sum) and the corresponding numerical elements that make that sum
#' @export

set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000)
  )

#largest value (sum of v's in the data frame) that is below the overall weight 


brute_force_knapsack<-function(x,W){
  if (!is.data.frame(x)){
    stop("x is not a data frame")
  }
  if (W<0){
    stop("W needs to be positive")
  }
    v<-x$v
    w<-x$w
    if (all(v>0)&all(w>0)){
      n<-nrow(x)
      name<-rownames(x)
      
      elementCombos<-list()
      weights<-list()
      values<-list()
      for (i in 1:n){
        elementCombos[[i]]<-combn(name,i,paste, collapse = "")
        weights[[i]]<-combn(w,i, FUN=sum)
        values[[i]]<-combn(v,i, FUN=sum)
      }
      
      maxweight<-which(unlist(weights)<=W)
      correspondingValues<-round(unlist(values))[maxweight]
      valuesmax<-max(correspondingValues)
      
      index<-which(round(unlist(values)) == valuesmax)
      correspondingElement<-unlist(elementCombos)[index]
      elements<-unlist(strsplit((correspondingElement),""))
      return(list(value=valuesmax,elements=as.numeric(elements)))
    }
    else
      stop("values are not positive")
}


#system time for n=16
library(profvis)
source("R/brute_force.R")
profvis(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500))