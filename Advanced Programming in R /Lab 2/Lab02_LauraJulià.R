name <- "Laura Julià Melis"
liuid <- "lauju103"

# 1.1.1
sheldon_game <- function(player1, player2){
  stopifnot(player1=="rock" || player1=="scissors" ||player1=="spock" || player1=="lizard" ||player1=="paper")
  stopifnot(player2=="rock" || player2=="scissors" ||player2=="spock" || player2=="lizard" ||player2=="paper")
  
  if(player1==player2){
    return("Draw!")
  }else if(player1== "scissors" && (player2== "rock" ||  player2=="spock")){
    return("Player 2 wins!")
  }else if(player1== "paper" && (player2== "scissors" ||  player2=="lizard")){
    return("Player 2 wins!")
  }else if(player1== "lizard" && (player2== "scissors" ||  player2=="rock")){
    return("Player 2 wins!")
  }else if(player1== "rock" && (player2== "paper" ||  player2=="spock")){
    return("Player 2 wins!")
  }else if(player1== "spock" && (player2== "paper" ||  player2=="lizard")){
    return("Player 2 wins!")
  }else{
    return("Player 1 wins!")
  }
}

sheldon_game("lizard", "spock")
sheldon_game("rock", "paper")

# 1.2.1
my_moving_median <- function(x, n,...){
  stopifnot(is.vector(x) && is.numeric(x) && is.numeric(n) && length(n)==1)
  y <- vector()
  for(i in 1:(length(x)-n)){
    y[i]<- median(x[i:(i+n)], na.rm = ...)
  }
  return(y)
}

my_moving_median(x = 1:10, n=2)
my_moving_median(x = 5:15, n=4)
my_moving_median(x = c(5,1,2,NA,2,5,6,8,9,9), n=2)
my_moving_median(x = c(5,1,2,NA,2,5,6,8,9,9), n=2, na.rm=TRUE)

# 1.2.2
for_mult_table <- function(from,to){
  stopifnot(length(from)==1 && length(to)==1)
  vector<- from:to
  mat<- matrix(nrow = length(vector),ncol=length(vector))
  rownames(mat) <- vector
  colnames(mat) <- vector
  for(i in 1:length(vector)){
    for(j in 1:length(vector)){
    mat[i,j] <- vector[i]*vector[j]
    }
  }
  return(mat)
}
for_mult_table(from = 1, to = 5)
for_mult_table(from = 10, to = 12)

# 1.2.3
cor_matrix <- function(df){
  stopifnot(is.data.frame(df))
  m <- matrix(NA, nrow=ncol(df), ncol=ncol(df))
  for(i in 1:nrow(m)){
    for(j in 1:ncol(m)){
      x <- df[,i]
      y <- df[,j]
      
      m[i,j] <- (sum((x-mean(x))*(y-mean(y))))/(sqrt(sum((x-mean(x))^2)*sum((y-mean(y))^2)))
    }
  }
  return(m)
}

data(iris)
cor_matrix(iris[,1:4])
data(faithful)
cor_matrix(faithful)

# 1.3.1
find_cumsum <- function(x, find_sum){
  stopifnot(is.numeric(x) && is.vector(x) && is.numeric(find_sum) && length(find_sum)==1)
  sum <- 0
  i <- 1
  while(sum <= find_sum & i <= length(x)){
    sum <- sum + x[i]
    i <- i +1
  }
  return(sum)
}

find_cumsum(x=1:100, find_sum=500)
find_cumsum(x=1:10, find_sum=1000)

# 1.3.2
while_mult_table <- function(from, to){
  stopifnot(length(from)==1 && length(to)==1)
  vector<- from:to
  mat<- matrix(nrow = length(vector),ncol=length(vector))
  rownames(mat) <- vector
  colnames(mat) <- vector
  
  i <- 1
  j <- 1
  while(i<=length(vector)){
    while(j<=length(vector)){
        mat[i,j] <- vector[i]*vector[j]
      j <- j + 1
    }
    i <- i + 1
    j <- 1
  }
  return(mat)
}

while_mult_table(from = 3, to = 5)
while_mult_table(from = 7, to = 12)

# 1.3.3
trial_division_factorization <- function(x){
  i <- 1; n <- 2; c <- x/n;  p <- x%/%n
  m <- vector()
  while(c!=1){
    c <- x/n
    p <- x%/%n
    if((c-p)==0){
      m[i] <- n
      i <- i+1
      n <- 2
      x <- c
    }else{n <- n+1}
  }
  return(m) 
}

trial_division_factorization(x = 2^3 * 13 * 17 * 31)
trial_division_factorization(x = 47 * 91 * 97)

# 1.4.1
repeat_find_cumsum <- function(x, find_sum){
  stopifnot(is.numeric(x) && is.vector(x) && is.numeric(find_sum) && length(find_sum)==1)
  sum <- 0
  i <-1
  repeat{
    sum <- sum + x[i]
    if (sum >= find_sum | i >= length(x)){return(sum);break}
    i <- i +1
  }
  return(sum)
}

repeat_find_cumsum(x=1:100, find_sum=500)
repeat_find_cumsum(x=1:10, find_sum=1000)

# 1.4.2
repeat_my_moving_median <- function(x, n,...){
  stopifnot(is.vector(x) && is.numeric(x) && is.numeric(n) && length(n)==1)
  y <- vector()
  i <- 1
  repeat{
    y[i]<- median(x[i:(i+n)], na.rm=...)
    if(i >= (length(x)-n)){return(y); break}
    i <- i+1
  }
  return(y)
}

repeat_my_moving_median(x = 1:10, n=2)
repeat_my_moving_median(x = 5:15, n=4)
repeat_my_moving_median(x = c(5,1,2,NA,2,5,6,8,9,9), n=2)

# 1.5.1
in_environment <- function(env){
  ls(env)
}
env <- search()[length(search())]
env

funs <- in_environment(env)
funs[1:5]

# 1.6.1
cov <- function(X){
    x<-lapply(1:ncol(X), function(i){sd(X[,i])/mean(X[,i])})
    sol<- vector()
    for(i in 1:ncol(X)){
      sol[i]<- x[[i]]
      names(sol)[i]<-names(X[i])
    }
    return(sol)
}

data(iris)
cov(X = iris[1:4])
cov(X = iris[3:4])

# 1.7.1
moment <- function(i){
  stopifnot(is.numeric(i))
  sol <- function(x){
    sum((x - mean(x)) ^ i) / length(x)
  }
  return(sol)
}


m1 <- moment(i=1)
m2 <- moment(i=2)
m1(1:100)
m2(1:100)