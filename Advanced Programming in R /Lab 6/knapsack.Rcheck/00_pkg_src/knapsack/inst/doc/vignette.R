## ---- message=FALSE, warning=FALSE---------------------------------------
# You may need to install and load this package first: install.packages("devtools")
library(devtools)

# devtools::install_github("laurajuliamelis/Lab6",subdir="knapsack")
# library(knapsack)

## ----include=FALSE-------------------------------------------------------
# We need this package to measure the time that it takes to evaluate the functions.
#install.packages("microbenchmark")
library(microbenchmark)

## ------------------------------------------------------------------------
set.seed(42)
n <- 2000
knapsack_objects <- data.frame( w=sample(1:4000, size = n, replace = TRUE),
                                v=runif(n = n, 0, 10000))

## ----include=FALSE-------------------------------------------------------
## THIS WILL BE REMOVED WHEN THE PACKAGE INSTALATION IS WORKING.
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

## ------------------------------------------------------------------------
brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)

## ------------------------------------------------------------------------
microbenchmark(brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500))
# RUN BEFORE SUBMITTING
# microbenchmark(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500))

## ----include=FALSE-------------------------------------------------------
## THIS WILL BE REMOVED WHEN THE PACKAGE INSTALATION IS WORKING.
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

## ------------------------------------------------------------------------
knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)

## ------------------------------------------------------------------------
microbenchmark(knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500))
# RUN BEFORE SUBMITTING: 
# microbenchmark(knapsack_dynamic(x = knapsack_objects[1:500,], W = 3500))

## ----include=FALSE-------------------------------------------------------
## THIS WILL BE REMOVED WHEN THE PACKAGE INSTALATION IS WORKING.
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

## ------------------------------------------------------------------------
greedy_knapsack(x = knapsack_objects[1:8,], W = 3500)

## ------------------------------------------------------------------------
set.seed(42)
n <- 1000000
knapsack_objects <- data.frame( w=sample(1:4000, size = n, replace = TRUE),
                                v=runif(n = n, 0, 10000))
# RUN BEFORE SUBMITTING
# microbenchmark(greedy_knapsack(x = knapsack_objects[1:1000000,], W = 3500))

microbenchmark(greedy_knapsack(x = knapsack_objects[1:8,], W = 3500))

