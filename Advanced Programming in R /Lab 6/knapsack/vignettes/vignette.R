## ---- message=FALSE, warning=FALSE---------------------------------------
# You may need to install and load this package first: install.packages("devtools")
library(devtools)

# devtools::install_github("laurajuliamelis/Lab6",subdir="knapsack")
library(knapsack)

## ------------------------------------------------------------------------
set.seed(42)
n <- 2000
knapsack_objects <- data.frame( w=sample(1:4000, size = n, replace = TRUE),
                                v=runif(n = n, 0, 10000))

## ------------------------------------------------------------------------
brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)

## ---- eval=FALSE---------------------------------------------------------
#  microbenchmark(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500), times = 5)

## ------------------------------------------------------------------------
knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)

## ---- eval=FALSE---------------------------------------------------------
#  microbenchmark(knapsack_dynamic(x = knapsack_objects[1:500,], W = 3500), times = 5)

## ------------------------------------------------------------------------
greedy_knapsack(x = knapsack_objects[1:8,], W = 3500)

## ---- eval=FALSE---------------------------------------------------------
#  set.seed(42)
#  n <- 1000000
#  knapsack_objects <- data.frame( w=sample(1:4000, size = n, replace = TRUE),
#                                  v=runif(n = n, 0, 10000))
#  
#  microbenchmark(greedy_knapsack(x = knapsack_objects[1:1000000,], W = 3500), times = 5)

## ---- eval=FALSE---------------------------------------------------------
#  profvis(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500))

## ---- eval=FALSE---------------------------------------------------------
#  profvis(knapsack_dynamic(x = knapsack_objects[1:500,], W = 3500))

## ---- eval=FALSE---------------------------------------------------------
#  profvis(greedy_knapsack(x = knapsack_objects[1:1000000,], W = 3500))

## ---- eval=FALSE---------------------------------------------------------
#  microbenchmark(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500), times = 5)

## ---- eval=FALSE---------------------------------------------------------
#  microbenchmark(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500, fast= TRUE), times = 5)

## ---- eval=FALSE---------------------------------------------------------
#  microbenchmark(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500), times = 5)

## ---- eval=FALSE---------------------------------------------------------
#  microbenchmark(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500, parallel = TRUE), times = 5)

## ---- eval=FALSE---------------------------------------------------------
#  microbenchmark(brute_force_knapsack(x = knapsack_objects[1:20,], W = 3500), times = 1)

## ---- eval=FALSE---------------------------------------------------------
#  microbenchmark(brute_force_knapsack(x = knapsack_objects[1:20,], W = 3500, parallel = TRUE), times = 1)

