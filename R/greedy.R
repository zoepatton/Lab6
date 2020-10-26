set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000))

#' Largest possible value in weighted bag - greedy
#'
#' This function takes the values and weights provided in a data frame and finds the largest possible sum of values that is less than or equal to the inputed weight by ordering each element by value per weight.  
#'
#' @param x data frame
#' @param W positive numeric value
#' @return The output will be a numerical value(the largest possible sum) and the corresponding numerical elements that make that sum
#' @export greedy_knapsack



greedy_knapsack <- function(x, W){
  
  
  if (!is.data.frame(x)){
    stop("x is not a data frame")
  }
  if (W<0){
    stop("W needs to be positive")
  }
  
  
  if(any(x$v > 0) & any(x$w > 0)){
    
    x <- x[order(x$v/x$w, decreasing = TRUE), ]
    max_weight <- W
    
    sack <- c()
    elements <- c()
    i = 1
    repeat{
      
      sack[i] <- x$v[i]
      elements <- rownames(x[1:(i-1),])
      i = i + 1
      
      if(sum(x$w[1:i]) > max_weight){break}
      
    }
    
  }
  else
    stop("values are not positive")
  
  
  greedy_sack <- round(sum(sack))
  greedy_list <- list(greedy_sack, as.numeric(elements))
  names(greedy_list) <- c("value", "elements")
  
  return(greedy_list)
}



