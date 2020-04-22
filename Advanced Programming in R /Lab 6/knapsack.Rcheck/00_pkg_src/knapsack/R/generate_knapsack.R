#' generate_backpack
#'
#' \code{generate_backpack} makes a GET request for data from the 
#' Swedish Parlament API, specifically for votations.
#' 
#' 
#' @param period a 
#' @param span b

#' @return 
#'
#' @examples
#' generate_backpack()
#' 
#'
#' @export
#' 

generate_backpack <- function(){
  # Data
  set.seed(42)
  n <- 2000
  knapsack_objects <-
    data.frame(
      w=sample(1:4000, size = n, replace = TRUE),
      v=runif(n = n, 0, 10000)
    )
  return(knapsack_objects)
}
