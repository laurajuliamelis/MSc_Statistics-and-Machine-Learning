# library(parallel)
# 
# pas <- function(i){
#   return(i)
# }
# 
# result <- c()  
# index <- 1:10  
# for (i in index) {  
#   result[[i]] <- pas(i)
# }
# 
# result2  <- lapply(index, pas)
# 
# 
# no_cores <- max(1, detectCores() - 1)
# cl <- makeCluster(no_cores, type="PSOCK") 
# registerDoParallel(cl)
# result <- foreach(i=1:10) %dopar% pas(i)
# stopCluster(cl)
