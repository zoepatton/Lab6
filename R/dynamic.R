#' Largest possible value in weighted bag - dynamic
#'
#' This function takes the values and weights provided in a data frame and finds the largest possible sum of values that is less than or equal to the inputed weight by iterating over all possible values of the weight.
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


knapsack_dynamic <- function(x,W){
  
  if (!is.data.frame(x)){
    stop("x is not a data frame")
  }
  if (W<0){
    stop("W needs to be positive")
  }
  
  
  
  if(all(x$v > 0) & all(x$w > 0)){
    
    v <- c(0,x[,2])
    w <- c(0,x[,1])  
    max_weight <- W+1
    n<-nrow(x)+1
    
    H <- matrix(0, nrow = n, ncol = max_weight)
    
    for(k in 0:max_weight){
      H[0, k] <- 0
    }
    
    for(j in 0:max_weight){
      for(i in 2:n){       
        
        if(j < w[i]){
          
          H[i,j] <- H[i-1, j]
          
        } 
        else{
          result <- max(H[i-1,j], (H[i-1, j-w[i]] + v[i]))
          H[i,j] <- result 
        }
      }
    }
    
    
    #find elements
    
    elements<-c()
    max_result <- max(result)
    
    repeat{
      
      back <- which(H == max_result, arr.ind = TRUE)[1,1]
      max_result <- max_result - v[back]
      elements <- c(as.numeric(back-1), elements)
      
      if(0 == max_result){break}
      
    }
    
    value <- round(max(result))
    final_list <- list(value, elements)
    names(final_list) <- c("value", "elements")
    
  }
  else
    stop("values are not positive")
  
  return(final_list)
}