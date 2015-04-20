# Gradient Descent using different stepsizes - Basics

grad_descent <- function(start_val=NA, f_name=NULL, grad_f_name=NULL, step_method="armijo", accuracy=1e-6, max_iterations=1000000,print_details=FALSE){
  
  # If fixed stepsize
  if (step_method == "fixed") {
    fixed_step = TRUE 
    fixed_stepsize = 1e-3
    # Some comments: If fixed_stepsize is chosen too large, then there might be "strange" error messages, try reducing fixed_stepsize
    # Even 1e-6 should produce an error for Rosenbrock function in n=7 with the average_point, but it should work with 1e-7
    # Don't forget to adjust the max_iterations parameter, as the number of steps might explode if fixed_stepsize is small
  } else fixed_step = FALSE
  
  print(paste("Stepsize method:", step_method))
  if (fixed_step) print(paste("WARNING: Might take very long, fixed stepsize =",fixed_stepsize))
                        
  # Set/compute initial values
  x = start_val # vector in R^n 
  gfx = grad_f_name(x) # gradient at x, vector in R^n

  # Advanced tolerance (for stopping criterion), the norm of the gradient will be bounded by a value in between 1e-12 and accuracy=1e-6 depending on the starting point
  # tol = max(1e-12, accuracy * min(1,sqrt(sum(gfx^2)))) # sqrt(sum((...)^2)) is the 2-norm of whatever is inside the inner (...)
  # tol = 1e-6 * sqrt(sum(gfx^2)) # relative tolerance, try if you are interested
  ### For safety reasons try this first
  tol = accuracy # absolute tolerance, default value 1e-6 
  iter = 0 # iteration counter
  
if (print_details) {
  print(paste("Internal Dimension:",length(x)))
  fx = f_name(x) # function value at point x, real number
  print(paste("Start at function value:",fx))
  print(paste("2-Norm of Gradient:",sqrt(sum(gfx^2))))
}

  # Parameters for Armijo
  sigma = 0.1 # should be < 0.5
  alpha = 0.5 # should be < 1.0

  # Start loop, check stopping/running conditions, stops if norm(gradient) <= tol
  #while (sqrt(sum(gfx^2)) > tol & iter < max_iterations) { # Euclidean norm (2-norm)
  while (max(abs(gfx)) > tol & iter < max_iterations) { # Maximum norm instead of Euclidean norm

  # Print iteration counter from time to time
  if (iter %% 100000 == 0 & iter > 0) print(paste("Iteration:",iter)) 
    
  if (fixed_step) {

    # FIXED STEPSIZE - Activate if you have a lot of time, in case of strange error messages reduce stepsize 
    t = fixed_stepsize

  } else {
    
    # Available stepsizes: Armijo's rule, Armijo with widening, Exact stepsize using a bisection method (needs gradient information, thus expensive) 
    if (step_method == "armijo") t = armijo(sig=sigma, al=alpha,x_val=x,grad_x_val=gfx,direction=-gfx,f_arm=f_name) # Armijo's rule
    else if (step_method == "armijo_wide") t = armijo_wide(sig=sigma, al=alpha,x_val=x,grad_x_val=gfx,direction=-gfx,f_arm=f_name) # Armijo with widening
    else if (step_method == "exact") t = bis_method(x,-gfx,grad_f_name) # Exact line search (bisection)
    else stop("Incorrect stepsize method!")
    
  }
  
    # Compute new iterate and gradient
    x = x - t * gfx
    gfx = grad_f_name(x)
    
  # increment counter
    iter = iter + 1
  
  } # end while loop
  
  count_iter <<- iter
  if (print_details) {
    print(paste("End at function value:",f_name(x)))
    count_f <<- count_f - 2 # Correct function counter
    print(paste("2-Norm of Gradient:", sqrt(sum(gfx^2))))
    }
  return(x)

}