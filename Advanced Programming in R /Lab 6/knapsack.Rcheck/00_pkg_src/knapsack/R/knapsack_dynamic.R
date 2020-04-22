#' The knapsack problem: Dynamic problem algorithm.
#'
#' The knapsack problem is a discrete optimization problem where we have a knapsack that can take a 
#' limited weight W and we want to fill this knapsack with a number of items i = 1,...,n, each with 
#' a weight w(i) and a value v(i). The goal is to find the knapsack with the largest value of the 
#' elements added to the knapsack.
#' 
#' \code{knapsack_dynamic} uses the dynamic programming in-advance algorithm. 
#' 
#' @param x an object of class data.frame with two variables v (values) and w (weights).
#' @param W numeric scalar object that represents the knapsack size.
#'
#' @return  \code{knapsack_dynamic} returns a list with two elements: the elements added to the knapsack and the maximum knapsack value.
#'
#' @examples
#' knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
#' knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
#' knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
#' knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)
#' 
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem#0.2F1_knapsack_problem}
#'
#' @export
#' 

knapsack_dynamic <- function(x, W){
  
  # Output equals 0 when there are no items to pick or the knapsack can hold 0 weight units
  if(nrow(x)==0 || W==0){
    return(list("value"=0, "elements"="None"))
  }
  
  # Create m array for the values
  m <- matrix(nrow = W+1 , ncol= nrow(x)+1) 
  m[1, ] <- 0 # Row 1 represents that the knapsack can hold 0 weight units
  m[, 1] <- 0 # Column 1 represents 0 items to pick up
  
  # Create m array for the elements.
  m_item<-matrix('', nrow =W+1 , ncol= nrow(x)+1)
  
  # When other input values:
  for (j in 1:nrow(x)){ # j in cols
    temp_weight<-x$w[j]
    item <- j
    temp_value <- x$v[j]
    
    for(i in 1:W){ # i in rows
      if(temp_weight > i){ 
       
        m[i+1,j+1] <- m[i+1,j]
        m_item[i+1,j+1] <- m_item[i+1,j]
      
      }else{ 
       
         if(m[i+1,j] >= m[i+1-temp_weight,j]+temp_value){
          m[i+1,j+1] <- m[i+1,j]
          m_item[i+1,j+1] <- m_item[i+1,j]
        }else{
          m[i+1,j+1] <- (m[i+1-temp_weight,j]+temp_value)
          m_item[i+1,j+1] <- item
        }
        
      }
    }
  }
  
  # Obtaining the chosen elements:
  n_row <-nrow(m)
  n_col<-ncol(m)
  items<-c()
  selected_item<-m_item[n_row,n_col]
  
  while(selected_item!='')
  {
    selected_item<-m_item[n_row,n_col]
    if(selected_item!='')
    {
      selected_item_value<-x[selected_item,]
      if(-m[n_row - selected_item_value$w,n_col-1]+m[n_row,n_col]==selected_item_value$v)
      {
        n_row <- n_row - selected_item_value$w
        items<-c(items,selected_item)
      }
      n_col <- n_col - 1
    }
  }
  return(list("value"=m[nrow(m),ncol(m)], "elements"=sort(as.numeric(items))))
}
