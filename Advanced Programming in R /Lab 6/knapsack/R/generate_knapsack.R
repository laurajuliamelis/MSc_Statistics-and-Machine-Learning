#' generate_knapsack
#'
#' \code{generate_knapsack} generates a dataset for example usage of the functions.
#' 
#' 
#' @param n number of samples to generate. 
#' @param seed which seed to use.
#' @return data.frame with the columns w and v.
#'
#' @examples
#' generate_knapsack()
#' 
#' @importFrom stats runif
#'
#' @export
#' 

generate_knapsack <- function(n = 2000, seed = 42){
  # Data
  set.seed(seed)
  knapsack_objects <-
    data.frame(
      w=sample(1:4000, size = n, replace = TRUE),
      v=runif(n = n, 0, 10000)
    )
  return(knapsack_objects)
}
