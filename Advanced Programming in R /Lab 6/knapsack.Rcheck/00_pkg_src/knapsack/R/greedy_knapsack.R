#' The knapsack problem: Greedy approximation algorithm.
#'
#' The knapsack problem is a discrete optimization problem where we have a knapsack that can take a 
#' limited weight W and we want to fill this knapsack with a number of items i = 1,...,n, each with 
#' a weight w(i) and a value v(i). The goal is to find the knapsack with the largest value of the 
#' elements added to the knapsack.
#' 
#' \code{greedy_knapsack} uses the Greedy approximation algorithm. This algorithm works by:
#' 
#' (i) Computing the value-to-weight ratios.
#' 
#' (ii) Sorting the items in decreasing order of the value-to-weight ratios
#' 
#' (iii) Placing the sorted items in the knapsack until the current weight is higher than the knapsack capacity W.
#'  
#' @param x an object of class data.frame with two variables v (values) and w (weights).
#' @param W numeric scalar object that represents the knapsack size.
#'
#' @return  \code{greedy_knapsack} returns a list with two elements: the elements added to the knapsack and the maximum knapsack value.
#'
#' @examples
#' greedy_knapsack(x = knapsack_objects[1:8,], W = 3500)
#' greedy_knapsack(x = knapsack_objects[1:12,], W = 3500)
#' greedy_knapsack(x = knapsack_objects[1:8,], W = 2000)
#' greedy_knapsack(x = knapsack_objects[1:12,], W = 2000)
#' 
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm}
#'
#' @export
#'

greedy_knapsack <- function(x, W){
  x$r <- x$v / x$w
  sort_x <- x[order(x$r, decreasing = T),]
  
  sum_weight <-0
  value <- 0
  elements <- vector()
  for(i in 1: nrow(sort_x)){

    sum_weight <- sum(sum_weight, sort_x$w[i])
    
    if(sum_weight < W){
      value <- sum(value, sort_x$v[i])
      elements <- c(elements, row.names(sort_x)[i])
    }else{
      return(list("value"=value, "elements"=sort(as.numeric(elements))))
    }
    
  }
}
