#' The knapsack problem: brute force algorithm.
#'
#' The knapsack problem is a discrete optimization problem where we have a knapsack that can take a 
#' limited weight W and we want to fill this knapsack with a number of items i = 1,...,n, each with 
#' a weight w(i) and a value v(i). The goal is to find the knapsack with the largest value of the 
#' elements added to the knapsack.
#' 
#' \code{brute_force_knapsack} uses the brute-force algorithm. This algorithms works by going through
#' all possible alternatives (all possible combinations 2n are evaluated) and return the maximum
#' value found.
#' 
#' @param x an object of class data.frame with two variables v (values) and w (weights).
#' @param W numeric scalar object that represents the knapsack size.
#'
#' @return  \code{brute_force_knapsack} returns a list with two elements: the elements added to the knapsack and the maximum knapsack value.
#'
#' @examples
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
#' 
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#'
#'
#' @export
#' 

brute_force_knapsack <- function(x, W){
  combn <- 1:(2^nrow(x))
  bin <- matrix(NA, nrow=2^nrow(x), ncol=nrow(x))
  for(i in 1:(2^nrow(x))){
    bin[i,]<- as.integer(head(intToBits(combn[i]), nrow(x))) 
  }
  
  weight <- W
  elements <- vector()
  value <- 0
  for(i in 1:nrow(bin)){
    temp_weight <- sum(x[,1][as.logical(bin[i,])])
   
     if(temp_weight <= W){
      temp_value <- sum(x[,2][as.logical(bin[i,])])
      
      if(temp_value > value || (temp_value == value && temp_weight < weight)){
        weight <- temp_weight
        value <- temp_value
        elements <- which(as.logical(bin[i,]))
      }
    }
  }
  
  return(list("value"=value, "elements"=elements))
}


