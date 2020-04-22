#' GET_votation
#'
#' \code{GET_votation} makes a GET request for data from the 
#' Swedish Parlament API, specifically for votations.
#' 
#' 
#' @param period a 
#' @param span b

#' @return 
#'
#' @examples
#' brute_force_knapsack2(x = knapsack_objects[1:8,], W = 3500)
#' brute_force_knapsack2(x = knapsack_objects[1:12,], W = 3500)
#' brute_force_knapsack2(x = knapsack_objects[1:8,], W = 2000)
#' brute_force_knapsack2(x = knapsack_objects[1:12,], W = 2000)
#' 
#' @references \url{http://data.riksdagen.se/}
#'
#' @import parallel
#' @import foreach
#' @import doParallel
#'
#' @export
#' 


# Function
brute_force_knapsack2 <- function(x, W){
  combn <- 1:(2^nrow(x))
  
  
  .body <- function(i, span, x, W){
    bin <- as.logical(head(intToBits(span[i]), nrow(x)))
    temp_weight <- sum(x[,1][bin])
    
    if(temp_weight <= W){
      return(c(temp_weight,
             sum(x[,2][bin])))
    }else{
      return(c(NA,NA))
    }
  }
  
  result <- list()
  
  no_cores <- max(1, detectCores() - 1)
  cl <- makeCluster(no_cores, type="PSOCK") 
  registerDoParallel(cl)
  result <- foreach(i=combn) %dopar% .body(i, combn, x, W)
  stopCluster(cl)
  
  # result <- list()
  # for(i in combn){
  #   result[[i]] <- .body(i, combn, x, W)
  # }
  result <- matrix(unlist(result), byrow = TRUE, ncol=2)
  result_index <- which.max(result[,2])
  value <- result[result_index, 2]
  elements <- which(as.logical(head(intToBits(result_index), nrow(x))))
  
  return(list("value"=value, "elements"=elements))
}


#Sys.info()['sysname'] == "Windows"