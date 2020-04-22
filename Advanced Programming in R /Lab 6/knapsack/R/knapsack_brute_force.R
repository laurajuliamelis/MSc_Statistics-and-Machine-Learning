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
#' @param parallel make the function run in parallel.
#' @param fast uses a c++ function for faster execution.
#' 
#'
#' @return  \code{brute_force_knapsack} returns a list with two elements: the elements added to the knapsack and the maximum knapsack value.
#'
#' @examples
#' knapsack_objects <- generate_knapsack()
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500, parallel=TRUE)
#' 
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#'
#' @import parallel
#' @importFrom utils head
#' 
#' @export
#' 

brute_force_knapsack <- function(x, W, parallel = FALSE, fast = FALSE){
  stopifnot(W > 0, is.data.frame(x), is.logical(parallel), is.logical(fast))
  
  combn <- 1:(2^nrow(x))
  
  #' @describeIn brute_force_knapsack Description in main function.
  .calculate_row <- function(itr, span, data_frame, weight, use_cpp){
    if(use_cpp){
      bin <- as.logical(intToBinary(span[itr], nrow(data_frame)))
    }else{
      bin <- as.logical(head(intToBits(span[itr]), nrow(data_frame)))
    }
    
    temp_weight <- sum(data_frame[,1][bin])
    
    if(temp_weight <= weight){
      return(c(temp_weight,
               sum(data_frame[,2][bin])))
    }else{
      return(c(NA,NA))
    }
  }
  
  result <- list()
  
  if(parallel){
    chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
    if (nzchar(chk) && chk == "TRUE") {
      # use 2 cores in CRAN/Travis/AppVeyor
      no_cores <- 2L
    }else{
      # Leave 1 core for other processes to not lock the computer.
      no_cores <- max(1, detectCores() - 1)
    }
   
    
    if(Sys.info()['sysname'] == "Windows"){
      cl <- makeCluster(no_cores, type="PSOCK") 
    }else{
      cl <- makeCluster(no_cores, type="FORK") 
    }
    result <- parLapply(cl=cl, X=combn, fun=.calculate_row, 
                        combn, x, W, use_cpp=fast, chunk.size = as.integer(combn/no_cores))
    stopCluster(cl)
  }else{
    for(i in combn){
      result[[i]] <- .calculate_row(i, combn, x, W, use_cpp=fast)
    }
  }
  
  result <- matrix(unlist(result), byrow = TRUE, ncol=2)
  result_index <- which.max(result[,2])
  value <- result[result_index, 2]
  elements <- which(as.logical(head(intToBits(result_index), nrow(x))))
  
  return(list("value"=value, "elements"=elements))
}
