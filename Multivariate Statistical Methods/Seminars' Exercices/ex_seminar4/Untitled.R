# EXERCISE 6.19
gasoline <- matrix(c(16.44, 12.43, 11.23, 7.19, 2.70, 3.92, 9.92, 1.35, 9.75, 4.24, 5.78, 7.78,
                     11.20, 5.05, 10.67, 14.25, 5.78, 9.88, 13.50, 10.98, 10.60, 13.32, 14.27,
                     9.45, 29.11, 15.09, 3.28, 12.68, 7.61, 10.23, 7.51, 5.80, 8.13, 9.90, 3.63,
                     9.13, 10.25, 5.07, 10.17, 11.11, 6.15, 7.61, 12.17, 14.26, 14.39, 10.24,
                     2.59, 6.09, 10.18, 6.05, 12.14, 8.88, 2.70, 12.23, 12.34, 7.73, 11.68, 8.51,
                     14.02, 12.01, 26.16, 17.44, 16.89, 12.95, 8.24, 7.18, 16.93, 13.37, 17.59,
                     14.70, 10.78, 14.58, 10.32, 5.16, 17.00, 8.98, 4.49, 4.26, 9.70, 11.59, 6.83,
                     12.72, 8.63, 5.59, 9.49, 2.16, 6.23, 8.22, 7.95, 6.72, 13.70, 11.22, 4.91, 8.21,
                     9.85, 8.17, 15.86, 11.42, 13.06, 9.18, 9.18, 9.49, 12.49, 4.67, 11.94, 17.32, 6.86, 4.44), ncol = 3, byrow = T)
diesel <- matrix(c(8.50, 12.26, 7.42, 5.13, 10.28, 3.32, 10.16, 14.72, 12.79, 4.17, 9.60, 12.72, 6.47, 8.89, 11.35, 9.95,
                   9.15, 2.94, 9.70, 5.06, 9.77, 17.86, 11.61, 11.75, 9.09, 13.25, 8.53, 10.14, 8.29, 6.22, 15.90, 12.90,
                   11.94, 5.69, 9.54, 16.77, 10.43, 17.65, 10.87, 21.52, 7.13, 13.22, 11.88, 12.18, 12.03, 9.22), ncol=2, byrow=T)
disel_col3 <- c(9.11, 17.15, 11.23, 5.99, 29.28, 11.00, 19.00, 14.53, 13.68, 20.84, 35.18, 17.00, 20.66, 17.45, 16.38, 19.09, 14.77, 22.66, 10.66, 28.47, 19.44, 21.20, 23.09)
diesel <- cbind(diesel, disel_col3)
names<- c("x1","x2", "x3")
colnames(gasoline) <- names
colnames(diesel) <- names

colMeans(diesel)
colMeans(gasoline)

inv <- solve((59/828)*Spooled)
gasoline <- gasoline[-c(9,21),]

# EXERCISE 6.22
# subquestion a
data <- read.table("T6-12.dat", stringsAsFactors = FALSE)
males <- data[1:25,-c(5)]
females <- data[26:nrow(data),-c(5)]

mean_F <- colMeans(females)
mean_M <- colMeans(males)
dif <- as.numeric(mean_F-mean_M)

s1 <- cov(females)
s2 <- cov(males)
spooled <- 0.5*s1+0.5*s2

inv <- solve(0.08*spooled)

linear_comb_vector <- round(solve(spooled)%*%t(t(dif)),3)

# EXERCISE 8.6
means <- c(155.6, 14.7)
S <- matrix(c(7476.45, 303.62, 303.62,26.19),nrow=2, byrow = T)

eigen(S)

# EXERCISE 8.10
data <- read.table("T8-4.dat", stringsAsFactors = FALSE)
S <- cov(data)

eigen(S) # for the principal components

# Bonferroni's

m=3
n=103
alpha <- 0.1
z <- qnorm(1- (alpha/(2*m)))

lambdas <- eigen(S)$values[1:3]

lower <- vector()
upper <- vector()
for(i in 1:3){
  lower[i] <- lambdas[i]/(1+(z*sqrt(2/n)))
  upper[i] <- lambdas[i]/(1-(z*sqrt(2/n)))
}
round(cbind(1:3, lower, upper),6)








