set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000)
  )

#' Largest possible value in weighted bag - dynamic
#'
#' This function takes the values and weights provided in a data frame and finds the largest possible sum of values that is less than or equal to the inputed weight by iterating over all possible values of the weight.
#'
#' @param x data frame
#' @param W positive numeric value
#' @return The output will be a numerical value(the largest possible sum) and the corresponding numerical elements that make that sum
#' @export knapsack_dynamic



knapsack_dynamic <- function(x,W){
  
  if (!is.data.frame(x)){
    stop("x is not a data frame")
  }
  if (W<0){
    stop("W needs to be positive")
  }
  
  
  
  if(all(x$v > 0) & all(x$w > 0)){
    
    v <- x[,2]
    w <- x[,1]  
    max_weight <- W
    n<-nrow(x)
    
    H <- matrix(0, nrow = n+1, ncol = max_weight+1)
    
    for(k in 0:max_weight){
      H[0, k] <- 0
    }
    
    for(i in 2:n){
      for(j in 0:max_weight){       
        
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
    
    max_result <- max(result)  
    elements <- c()
    
    repeat{
      
      back <- matrix(which(H == max_result, arr.ind = T),1,1)
      max_result <- max_result - v[back]
      
      elements <- c(back, elements)
      
      
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

