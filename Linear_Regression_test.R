# Testscript for Linear regression
# https://github.com/cc-skuehn/Optimization

# Further explanations can be found here:
# https://blog.codecentric.de/en/2016/01/machinery-linear-regression/ (published January XXX, 2016)
# https://blog.codecentric.de/en/2015/04/machinery-part2/ (published April 21st, 2015)
# https://blog.codecentric.de/en/2015/03/machinery-part1/ (published March 19th, 2015)

########################
# Start preparation
########################

# Source related files, it is assumed that all relevant source files are in the current working directory
#source(paste(getwd(),'/stepsizes.R',sep=""))
#source(paste(getwd(),'/grad_descent.R',sep=""))
#source(paste(getwd(),'/lin_reg_cost_grad.R',sep=""))


########################
# Part 1: Your choices
########################

# Activate detailed output (start/end point, function value + norm of gradient)
details = TRUE # TRUE / FALSE

# Set seed for reproducible results
set.seed(123)

# Choose problem dimensions (nof_datapoints should be larger than nof_variables)
nof_datapoints = 100
nof_variables = 1 # WARNING: This number is n, i.e. without the parameter c_0, only c_1 to c_n are taken into account

### Create artificial Linear Regression problem ###

print("Linear Regression")

# Initialize data matrix X randomly (uniform at random, min=0, max=1), depends on the above seed
X <<- matrix(runif(nof_datapoints * nof_variables), ncol=nof_variables)
# Add first column of 1's
X <<- cbind(as.matrix(rep(1,nof_datapoints),ncol=1), X)

# Prepare Y as global variable and as sum of two parts:
# The first summand is determined by X and the vector "approximate_solution" (see explanation below)
# The second summand represents some noise, weighted by "noise_weight"
# If "noise_weight" is close to zero, then "approximate_solution" is going to be an approximate solution to the Linear Regression problem
approximate_solution = 1:(nof_variables+1)  # (1,2,3,...), feel free to change it
noise_weight = 0.5
noise_vec = runif(nof_datapoints)
noise_vec = noise_vec/sqrt(sum(noise_vec*noise_vec))
additional_noise = noise_weight * noise_vec # depends on the above random seed

# Initialize dependent variable
Y <<- X %*% approximate_solution + additional_noise

# Choose initial parameter values -> in the blog post this is the vector c 
Theta = rep(0,nof_variables+1)
#Theta = runif(nof_variables+1) # Random start, this would depend on the random seed as well 

### Choose stepsize method: armijo, armijo_wide, exact (bisection), fixed (WARNING: small value, can diverge, might take very long in higher dimensions and for Rosenbrock function)

stepsize = "armijo"
#stepsize = "armijo_wide"
#stepsize = "exact"
#stepsize = "fixed" # If you like change value in grad_descent.R

#########################
### Internal Optimization
#########################

# Perform minimization for the given parameter setting:
initial_values = Theta
fun_name = lin_reg_cost
grad_name = lin_reg_grad
accuracy=1e-6

#######################
# Gradient Descent
#######################

# Initialize counters as global variables
# Iterations, Function and Gradient Evaluations
count_iter <<- 0
count_f <<- 0
count_g <<- 0
# Number of successful widening steps in armijo_wide()
count_wide <<- 0
# Counters for bisection method (exact stepsize only)
count_bi1 <<- 0
count_bi2 <<- 0

print("Start Gradient Descent")
graddedsc_runtime<-system.time(res <- grad_descent(start_val=initial_values, f_name=fun_name, grad_f_name = grad_name, step_method=stepsize, accuracy=accuracy, print_details=details))

# Performance Overview
if (stepsize == "exact") {
  pr_res <- data.frame(graddedsc_runtime[1],count_iter,count_f,count_g,count_bi1,count_bi2,row.names=NULL)
  colnames(pr_res) <- c("Runtime:", "Iterations:", "Function:", "Gradient:", "Interval:","Bisection:")
} else if (stepsize == "armijo_wide") {
  pr_res <- data.frame(graddedsc_runtime[1],count_iter,count_f,count_g,count_wide,row.names=NULL)
  colnames(pr_res) <- c("Runtime:", "Iterations:", "Function:", "Gradient:", "Widening:")
} else if (stepsize == "armijo"){
  pr_res <- data.frame(graddedsc_runtime[1],count_iter,count_f,count_g,row.names=NULL)
  colnames(pr_res) <- c("Runtime:", "Iterations:", "Function:", "Gradient:")
} else if (stepsize == "fixed") {
  pr_res <- data.frame(graddedsc_runtime[1],count_iter,count_f,count_g,row.names=NULL)
  colnames(pr_res) <- c("Runtime:", "Iterations:", "Function:", "Gradient:")
} else stop('Stepsize not correct')
print(pr_res)

# end Gradient Descent
