## KB 15.5p C
#### PROBLEM 1
## KB 5p
## SUB-QUESTION A
## KB 3p
# For a good coding practice, the programmers should always have in mind that, apart from writing a code that works correctly, it has to be 
# easy to read and understand (for him in the future but also for the others). This means that it should be consistent (same structure, format and style, 
# descriptive names for the variables, etc). Also it's a good practice to code dividing the main task in small different functions: working on one step at a
# time allows the programmer make corrections during the course of the work (this can be done using a version control system as GitHub, for example). Finally, 
# testing one's own programs by using a unit testing library (as testthat in R, for instanće) is also a really good practice in computing because one can have
# constant feedbak about the coding and verify that it's a valid code (or that it is not, and correct it).


## SUB-QUESTION B
## KB 2p
# There are three crucial things that should be provided in a function's documentaation:
# (1) The inputs (or parameters) of the functions. It should be indicated the name and a brief description (type of object, what does it represent, ...)
# (2) The output (or autputs) from the function. Althought it is not mandatory, it is better to explain the class of the output or describe it a little bit.
# (3) Some examples. It is very important to provide examples in order to ilustrate how to use the function (even examples with errors to show what not to do).

# With roxygen this is done by using the tags @param, @return and @example. Also, the documentation should have a meaningul 
# title (written in the first line of the roxygen comments #') and a description section where it is described what the function does.


#### PROBLEM 2
## KB 6p
## SUB-QUESTION A
## KB 4p
create_fridge <- function(number_users){
  ## KB you have food 1 initially conflicting with itself
  ## bad initial value
  fridge <- list()
  for ( i in 1:number_users){
    fridge[i] <- list("Food items"=data.frame(food_ID=1:5, permissible_num = c(2,6,3,1,4), already_ate_today= rep(0, 5), not_together = as.vector(rep("1, 2", 5))))
  }
  names <- paste("User", 1:number_users)
  names(fridge)<- names
  class(fridge) <- "fridge"
  
  # Controls:
  for( i in 1:number_users){
    # food_ID has to be integer and unique
    if(anyDuplicated(fridge[[i]][,1])>0){
      index <- min(which(duplicated(fridge[[i]][,1])))
      fridge[[i]][index,] <- NULL # Just in case that there is any duplicated item, the oldest row created will be eliminated (because it will be the oldest information)
      
    }else if(any((fridge[[i]][,1])- round(fridge[[i]][,1]) != 0)){
      stop("This food item ID is not correct")
    }
  }
  return(fridge)
}


modern_fridge <- create_fridge(number_users=2)
modern_fridge

## SUB-QUESTION B
## KB 1p
update_food_list <- function(modern_fridge, user_id, food_id, update_max = 0, update_conflicting_food){
  
  index <- which(modern_fridge[[user_id]][,1] == food_id)
  
  if(update_max != 0){
    modern_fridge[[user_id]][index,2] <- update_max
  }
    #modern_fridge[[user_id]][index,4] <- update_conflicting_food
  return(modern_fridge)
}

user_id<-1
food_id<-1
update_max<-3
update_conflicting_food<-c(-2,3)

modern_fridge <-update_food_list(modern_fridge,user_id,food_id, update_max,update_conflicting_food)
modern_fridge
## KB does not see to change the conflicting food list


## SUB-QUESTION C
## KB 2p
request_food <- function(modern_fridge, user_id, foods_ids){
  for(i in foods_ids){
    
    if(modern_fridge[[user_id]][i,3] < modern_fridge[[user_id]][i,2]){
      modern_fridge[[user_id]][i,3] <- modern_fridge[[user_id]][i,3] + 1
    } else{
      return(paste("You don't have permission to eat another item of the following item:", "Item", i))
    }
  }
  return(modern_fridge)
}

user_id<-1
food_ids<-1:3

modern_fridge <-request_food(modern_fridge,user_id, food_ids)
modern_fridge
##> modern_fridge <-request_food(modern_fridge,user_id, food_ids)
## > modern_fridge <-request_food(modern_fridge,user_id, food_ids)
## > modern_fridge <-request_food(modern_fridge,user_id, food_ids)
## > modern_fridge <-request_food(modern_fridge,user_id, food_ids)
## > modern_fridge <-request_food(modern_fridge,user_id, food_ids)
## Error in modern_fridge[[user_id]][i, 3] : incorrect number of dimensions
## > modern_fridge
## [1] "You don't have permission to eat another item of the following item: Item 1"

## SUB-QUESTION D
## KB 0p
## this function is not connected to the fridge class
## and it overwrites base::print()
print <- function(modern_fridge, user_id){

      modern_fridge[[user_id]][5] <- modern_fridge[[user_id]][2]- modern_fridge[[user_id]][3]
      names(modern_fridge[[user_id]])[5] <- "items_left"
      df <- as.data.frame( modern_fridge[[user_id]])
      cat("You are permitted to still consume the following items:", "\n")
      return(df)
}

print(modern_fridge,2)


# library(ggplot2)
# plot<- function(modern_fridge, user_id){
#   modern_fridge[[user_id]][5] <- modern_fridge[[user_id]][2]- modern_fridge[[user_id]][3]
#   names(modern_fridge[[user_id]])[5] <- "items_left"
#   df <- as.data.frame( modern_fridge[[user_id]])
#   plot1 <- ggplot(df) + geom_point(aes(x=df$food_ID, y=df$items_left ))
#   print(plot1)
# }
# 
# plot(modern_fridge, 1)



#### PROBLEM 3
## KB 4.5p
## SUB-QUESTION A
## KB 1.5p
factorial_fun <- function(n){
  stopifnot(is.numeric(n), n > 0, (n-round(n)) == 0)
  vector <- 1:n
  
  if(n == 0){
    sol <- 1
  }else{
    sol <- 1
    for(i in 1:n){
      sol <- sol*vector[i]
    }
  }
  return(sol)
}


## KB 
## > factorial_fun("a")
## Error in round(n) : non-numeric argument to mathematical function
## > factorial_fun(c())
## Error in round(n) : non-numeric argument to mathematical function

## SUB-QUESTION B
## KB 1p
# The complexity is linear, the number of required multiplication opertions depends on the value of n. 
# So we can write this in Big-Oh notation as: O(n). 

## SUB-QUESTION C
## KB 2p
stirling_aprox <- function(n){
  stopifnot(is.numeric(n), n > 0, (n-round(n)) == 0)
  sol <- sqrt(2*pi*n)*(n/exp(1))^n
  return(sol)
}

stirling_aprox(n=10)

library(testthat)
test_that("Comparison of both functions with theoretical result", {
  n <- 3
  lower_tol <- sqrt(2*pi)*(n^(n+0.5))*exp(1)^(-n)
  expect_equal(stirling_aprox(n), lower_tol)
  
  upper_tol <- exp(1)*(n^(n+0.5))*exp(1)^(-n)
  expect_true(lower_tol <= factorial_fun(n))
  expect_true(factorial_fun(n) <= upper_tol)
})
