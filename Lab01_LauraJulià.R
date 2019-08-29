# Name: Laura Julià Melis 
# LIU-ID: lauju103

# 1.1.1
my_num_vector <- function(){
  return(round(c(log10(11), cos(pi/5),exp(pi/3), (1173 %% 7)/19),5))
}
my_num_vector()

# 1.1.2
filter_my_vector <- function(x, leq){
  for (i in 1:length(x)){
    if(x[i]>=leq){
       x[i]<- NA
    }
  }
  return(x)
}

filter_my_vector(x = c(2, 9, 2, 4, 102), leq = 4)

# 1.1.3
dot_prod <- function(a,b){
 return(sum(a*b))
}
dot_prod(a = c(3,1,12,2,4), b = c(1,2,3,4,5))
dot_prod(a = c(-1,3), b = c(-3,-1))

# 1.1.4
approx_e <- function(N){
  return(sum(1/factorial(0:N)))
}

approx_e(N = 2)
approx_e(N = 4)

e <- 0
n <- 1
while(i < 50){
  e <- round(approx_e(N = n),5)
  if (e == round(exp(1),5)){
    print(n)
    break
  }
  n <- n + 1
}
round(approx_e(N = 8),5) == round(exp(1),5) # TRUE, n needs to be 8

# 1.2.1
my_magic_matrix <- function(){
  return(matrix(c(4,9,2,3,5,7,8,1,6), nrow=3,byrow = TRUE))
}

my_magic_matrix() # all numbers are different and each row, column and diagonal sum equals 15.

# 1.2.2
calculate_elements <- function(A){
  return(dim(A)[1]*dim(A)[2])
}

mat <- my_magic_matrix()
calculate_elements(A = mat)

new_mat <- cbind(mat, mat)
calculate_elements(A = new_mat)

# 1.2.3
row_to_zero <- function(A, i){
  A[i,] <- 0
  return(A)
}

row_to_zero(A = mat, i = 3)
row_to_zero(A = mat, i = 1)

# 1.2.4
add_elements_to_matrix <- function(A, x, i, j){
  for(s in 1:length(i)){
    for(t in 1:length(j)){
      A[i[s],j[t]] <- sum(A[i[s],j[t]],x)
    }
  }
  return(A)
}

add_elements_to_matrix(A = mat, x = 10, i = 2, j = 3)
add_elements_to_matrix(A = mat, x = -2, i = 1:3, j = 2:3)

# 1.3.1
my_magic_list <- function(){
  list(info="my own list", my_num_vector(), my_magic_matrix())
}

my_magic_list()

# 1.3.2
change_info <- function(x, text){
  x$info <- text
  return(x)
}

a_list <- my_magic_list()
change_info(x = a_list, text = "Some new info")

# 1.3.3
add_note <- function(x, note){
  return(c(x, note=note ))
}

add_note(x = a_list, note = "This is a magic list!")

# 1.3.4
sum_numeric_parts <- function(x){
    sol <- 0
    for(i in 1:length(x)){
      if(is.numeric(x[[i]])){
        sol <- sol + Reduce("+",x[[i]])
      }
    }
  return(sol)
}
a_list <- my_magic_list()
sum_numeric_parts(x = a_list)
sum_numeric_parts(x = a_list[2])

# 1.4.1
my_data.frame <- function(){
  return(data.frame(id=c(1,2,3), name=c("John","Lisa","Azra"), income=c(7.3,0.00,15.21), rich=c(FALSE, FALSE,TRUE))) 
}

my_data.frame()

# 1.4.2
sort_head <- function(df, var.name, n){
  return(df[order(df[var.name], decreasing = T),][1:n,])
}
data(iris)
sort_head(df = iris, var.name = "Petal.Length", n = 5)

# 1.4.3
add_median_variable <- function(df, j){
  median(df[,j])
  df$compared_to_median <- 0
  for(i in 1: nrow(df)){
    if(df[i,j]== median(df[,j])){
      df[i, "compared_to_median"] <- "Median"
    }else if(df[i,j] < median(df[,j])){
      df[i, "compared_to_median"] <- "Smaller"
    }else{
      df[i, "compared_to_median"] <- "Grater"
    }
  }
  return(df)
}

data(faithful)
head(add_median_variable(df = faithful, 1))
tail(add_median_variable(df = faithful, 2))

# 1.4.4
analyze_columns <- function(df, j){
  v1 <- c(round(mean(df[,j[1]]),4), round(median(df[,j[1]]),4), round(sd(df[,j[1]]),4))
  v2 <- c(round(mean(df[,j[2]]),4), round(median(df[,j[2]]),4), round(sd(df[,j[2]]),4))
  names<- c("mean","median","sd")
  names(v1) <- names
  names(v2) <- names
  sol <- list(v1,v2,round(cor(df[,c(j[1],j[2])]),5))
  names(sol)<- c(colnames(df)[j[1]],colnames(df)[j[2]],"correlation_matrix")
  return(sol)
}

analyze_columns(df = faithful, 1:2)
analyze_columns(df = iris, c(1,3))
analyze_columns(df = iris, c(4,1))
