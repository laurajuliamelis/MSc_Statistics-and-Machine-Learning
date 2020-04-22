# Example to obtain Q and R from a given matrix A. (by hand)
A <- matrix(c(12,6,-4,-51,167,24,4,-68,-41), ncol=3)
Q <- matrix(nrow=nrow(A),ncol=ncol(A))
R <- matrix(0, nrow=nrow(A),ncol=ncol(A))

for (j in 1:ncol(A)) {
  v = A[,j] # Step 1 of the Gram-Schmidt process v1 = a1

  if (j > 1) {
    for (i in 1:(j-1)) {
      R[i,j] <- t(Q[,i]) %*% A[,j] # Find the inner product (noted to be Q^T a earlier)
      v <- v - R[i,j] * Q[,i] # Subtract the projection from v which causes v to become perpendicular to all columns of Q
    }      
  }
  
  R[j,j] <- sqrt(sum(v^2))# Find the L2 norm of the jth diagonal of R
  Q[,j] <- v / R[j,j] # The orthogonalized result is found and stored in the ith column of Q.
}

Q
R

# Easiest way to obtain Q and R.
qr_A <- qr(A)
Q <- qr.Q(qr_A)
R <- qr.R(qr_A)

# Obtaining the least-square solution in another example:
A <- matrix(c(0,0.06,0.14,0.25,0.31,0.47,0.60,0.70,rep(1,8)), ncol=2)
b <- matrix(c(0,0.08,0.14,0.20,0.23,0.25,0.28,0.29), ncol=1)
qr_A <- qr(A)

Q <- qr.Q(qr_A)
R <- qr.R(qr_A)

beta_hat <- as.vector(backsolve(R, (t(Q) %*% b)))
beta_hat

# Obtaining the least-square solution with our data:
formula=Petal.Length~Species
data=iris
x <- model.matrix(formula, data) # X matrix (independent variables)
name_dep <- all.vars(formula)[1] # Dependent variable/s name/s
y <- data[, name_dep] # y (dependent variable/s)

qr_x <- qr(x)
Q <- qr.Q(qr_x)
R <- qr.R(qr_x)
Q
R

# Regression coefficients:
beta_hat <- as.vector(backsolve(R, (t(Q) %*% y)))

# The fitted values:
y_hat <- x %*% beta_hat

# The residuals:
e_hat <- y - (x %*% beta_hat)

# The degrees of freedom:
df <- nrow(data) - ncol(x) # number of observations - number of parameters in the model

# The residual variance:
sigma2_hat <- as.numeric((t(e_hat)%*%e_hat)/df)

# The variance of the regression coefficients:
var_hat <- diag(solve(t(R)%*%R) * sigma2_hat) # Var(beta_hat)= (R^T * R)^(-1) * sigma_hat^2

# The t-values for each coefficient:
t_values <- beta_hat / std_error

# The p-values for each coefficient:
p_values <- 2*abs(pt(t_values, df, log.p = T))
  
  
  
