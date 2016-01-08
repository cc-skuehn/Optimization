# Cost function and gradient for Linear Regression algorithm

# X is the Feature Matrix plus one additional column of 1's for theta_0; columns j=0,...,n
# Y is the Dependent Variable (Y_i, i=1,...,m)
# X and Y are defined globally
# theta (theta_j, j=0,...,n) is the Parameter Vector of the Linear Regression model

# Function calls are counted by global variables count_f, count_g 

#lin_reg_cost <- function(X,Y,theta){
lin_reg_cost <- function(theta){
  
  # Increment function call counter
  count_f <<- count_f + 1
  # Dimensions of the matrices
  np1 = dim(X)[2] # n plus 1
  m = dim(X)[1]
  
  # Auxiliary vector, residual res=X*theta-Y
  res =  X %*% theta - Y
  # Cost function
  J = 1/(2*m) * t(res) %*% res
  
} # end linear regression cost function

#lin_reg_grad <- function(X,Y,theta){
lin_reg_grad <- function(theta){
    
  # Increment gradient call counter
  count_g <<- count_g + 1
  # Dimensions of the matrices
  np1 = dim(X)[2]
  m = dim(X)[1]
  
  temp_vec = rep(0,np1)
  
  # Auxiliary vector, residual res=X*theta-Y
  res =  X %*% theta - Y # length m
  #print(X)
  #print(theta)
  #print(Y)
  #print(X%*%theta)
  #print(X%*%theta-Y)
  #Sys.sleep(3)
  # Gradient
  for (i in 1:m) {
    temp = res[i]
    for (j in 1:np1) {
      temp_vec[j]=temp_vec[j]+temp*X[i,j]
    }
  }
  temp_vec = 1/m * temp_vec
  #cat("Theta", theta,"\n")
  #cat("temp_vec", temp_vec,"\n")
  #print(temp_vec)
  #Sys.sleep(0.2)
  grad = temp_vec
  
  
} # end linear regression gradient