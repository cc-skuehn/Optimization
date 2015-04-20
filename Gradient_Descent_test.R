# Testscript for Optimization Routines related to codecentric Blog Series
# Stefan Kühn, April 2015
# https://github.com/cc-skuehn/Optimization

# Further explanations can be found here:
# https://blog.codecentric.de/en/2015/03/machinery-part1/ (published March 19th 2015)
# https://blog.codecentric.de/en/2015/04/machinery-part2/ (published April 21st 2015)

########################
# Start preparation
########################

######
# Source related files, it is assumed that all relevant source files are in the current working directory
source(paste(getwd(),'/stepsizes.R',sep=""))
source(paste(getwd(),'/grad_descent.R',sep=""))
source(paste(getwd(),'/test_functions.R',sep=""))

########################
# Part 1: Your choices
########################

# Activate detailed output (nothing spectacular, just start/end point, function value + norm of gradient)
details = TRUE # TRUE / FALSE (or T / F)

# Choose test function: 1,2,3,4
testfun = 3

# Choose problem dimension
n = 7

# Choose stepsize method: armijo, armijo_wide, exact (bisection), fixed (WARNING: small value, can diverge, might take very long in higher dimensions and for Rosenbrock function)
stepsize = "armijo"
#stepsize = "armijo_wide"
#stepsize = "exact"
#stepsize = "fixed" # If you like change value in grad_descent.R

# Choose initial values
if (is.element(testfun,c(1,2,3))){
  
  # f_1 to f_3: Minimum is at (0,...0)
  # Three different choices for the initial value, try out a few others if you like
  bad_start = 100*(1:n)/n 
  average_start = -100*(1:n)/(10*n)
  good_start = rep(0.1,n)
  
  ### Your choice, feel free to trey something else
  initial_values = average_start

  } else if (testfun == 4){
  
  # f_4: Rosenbock function, minimum is at (1,...,1)
  # Three different choices for the initial value
  bad_start = 100*(1:n)/n # takes very long to converge
  average_start = -100*(1:n)/n + 2 # same distance to minimum in coordinate space as bad_start (different but similar function value and gradient), but way faster convergence (except some special cases)
  good_start = rep(0.9,n) # rather close to the solution
  
  ### Your choice, feel free to trey something else
  initial_values = average_start
  
} else stop("Check parameter testfun!")

############################################################################
### Part 2: Script, do not change unless you know what you are doing ;-)
############################################################################

# Prepare everything else (functions, counters)
# Function and Gradient definitions: test_functions.R
if (testfun==1) {
  print("Testfunction f_1, trivial, quadratic, radial")
  fname = f_test
  grname = grad_test
  hesname = hesse_test
} else if (testfun==2) {
  print("Testfunction f_2, trivial, quadratic, but anisotropic")
  fname = aniso_test
  grname = aniso_grad_test
  hesname = aniso_hesse_test
} else if (testfun==3) {
  print("Testfunction f_3, symmetric, 4th power")
  fname = sq_test
  grname = sq_grad_test
  hesname = sq_hesse_test
} else if (testfun==4){
  print("Testfunction f_4, Rosenbrock, massively anisotropic, 4th power")
  fname = rosenbrock_advanced
  grname = grad_rosenbrock_advanced
}

# Initialize counters as global variables
# Iterations, Function and Gradient Evaluations
count_iter <<- 0
count_f <<- 0
count_g <<- 0
# Number of successful widening steps in armijo_wide()
count_wide <<- 0
# Counters for bisection method
count_bi1 <<- 0
count_bi2 <<- 0

#######################
# End preparation
#######################

#######################
# Gradient Descent
#######################
print("Start Gradient Descent")
graddedsc_runtime<-system.time(res <- grad_descent(start_val=initial_values, f_name=fname, grad_f_name = grname, step_method=stepsize, print_details=details))
  
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
