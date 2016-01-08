### Stepsize rules

### Part 1 - Inexact
# Armijo rule
# Armijo rule with widening
### TODO (maybe)
# Wolfe-Powell
# Wolfe-Powell strict

### Part 2 - Exact
# Bisection
### For Experts
# Secant (can fail)
# Newton-Raphson (requires Hessian)

#########

# Armijo rule, line search
# sig: "Relaxation" parameter (sigma), sig < 0.5
# al: Stepsize parameter (alpha), al < 1.0
armijo = function(sig=0.1, al=0.5, x_val=NA, grad_x_val=NA, direction=NA, f_arm=NULL){
  
  fx = f_arm(x_val)
  j = 0
  f_new = f_arm(x_val + al^j*direction)
  slope = sig * t(grad_x_val) %*% direction
  while (f_new > (fx + al^j * slope)  & j<101) {
    j=j+1
    f_new = f_arm(x_val + al^j*direction)    
  }
  
  if (j>=100) print("Warning, stepsize very small!")
  # Return stepsize
  return(al^j)
 
}

# Armijo rule with widening, line search
# sig: "Relaxation" parameter (sigma), sig < 0.5
# al: Stepsize parameter (alpha), al < 1.0
armijo_wide = function(sig=0.1, al=0.5, x_val=NA, grad_x_val=NA, direction=NA, f_arm=NULL){
  
  fx = f_arm(x_val)
  j = 0
  f_new = f_arm(x_val + al^j*direction)
  slope = sig * t(grad_x_val) %*% direction
  while (f_new > (fx + al^j * slope)  & j<101) {
    j=j+1
    f_new = f_arm(x_val + al^j*direction)    
  }
  # Widening, if j=0 is accepted
  if (j==0){
    j=j-1
    f_new = f_arm(x_val + al^j*direction)    
    while (f_new < (fx + al^j * slope)) {
      count_wide <<- count_wide + 1 
      j=j-1
      f_new = f_arm(x_val + al^j*direction)    
    }
    j=j+1 # Last j was too optimistic, test failed
  }
  
  if (j>=100) print("Warning, stepsize very small!")
  # Return stepsize
  return(al^j)
  
}

### Part 2 - Exact

# Bisection method for solving the one-dimensional line search problem
# Finds the root of the directional derivative of a multivariate function
# Can be used for univariate functions as well, just set direction to 1
# If the initial function values have opposite sign -> there exists root in the interval
# If not, enlarge the interval (the "left" side of the interval remains fixed, x_1=0, preparation step)
# Convergence rate is linear as the interval is halved in each step
# One can improve the method by choosing other points than the midpoints of the current interval (not implemented)
# WARNING: Bisection stopping criterion btol can cause problems, especially in higher dimensions
# In case of bisection errors try reducing btol to 1e-16
# Two global counters: count_bi1 for preparation steps (find admissable start interval), count_bi2 for bisection steps (actually finding the root)
bis_method <- function(start_val=NA,direction=NA,grad_f_name=NULL,accuracy=1e-6,max_iterations=1000){
  
  ### Preparation of the bisection method
  
  # Counter for number of "find appropriate interval" operations
  count_bis = 0
  # Find initial values with opposite-signed function values (directional derivative)!
  iter = 0
  x_1 = 0
  x_2 = 1
  x_new = 0.5*x_2
  
  fx_1 = t(direction) %*% grad_f_name(start_val+x_1*direction)
  if (fx_1>0) {
    print(paste("Directional derivative:",fx_1[1]))
    stop("Directional derivative positive due to numerical errors. Try reducing btol.")
  }
  fx_2 = t(direction) %*% grad_f_name(start_val+x_2*direction)
  
  # Find admissible interval
  while (sign(fx_1)==sign(fx_2)) {
    count_bis <- count_bis + 1
    if (count_bis > 50) {
      print("Error in Bisection method (try adjusting btol):")
      print(paste(x_1,x_2))
      print(paste(fx_1,fx_2))
      stop("Could not find appropriate interval")
    }
    x_2 = 2 * x_2 # enlarge interval
    fx_2 = t(direction) %*% grad_f_name(start_val+x_2*direction)   
  }
  # Update global counter for bisection preparation steps
  count_bi1 <<- count_bi1 + count_bis
  
  # Prepare Bisection
  x_new = (x_1+x_2)/2
  fx_new = t(direction)%*%grad_f_name(start_val+x_new*direction)
  gfx = fx_new
  
  # Stopping criterion, the norm of the gradient is bounded by a value in between 1e-14 and accuracy=1e-6 depending on the starting point
  #btol = max(1e-14, accuracy * min(1,sqrt(sum(gfx^2)))) # sqrt(sum(...)^2) is the 2-norm of whatever is inside the (...)
  # try this first, exact line search needs high accuracy
  btol = 1e-12
  
  ### Bisection method
  while (iter < max_iterations+count_bis) {
    if (abs(fx_new) < btol | abs(x_1-x_2) < btol  ) {
      count_bi2 <<- count_bi2 + iter
      return(x_new)
    } else {
      iter = iter + 1
      x_new = (x_1+x_2)/2
      fx_new = t(direction)%*%grad_f_name(start_val+x_new*direction)
      if (sign(fx_new)==sign(fx_1)) {
        x_1 = x_new
        fx_1 = fx_new
      } else {
        x_2 = x_new
        fx_2 = fx_new
      }
    }
  }
  
  # In case of max_iterations
  count_bi2 <<- count_bi2 + iter
  return(x_new)
  
} # end bisection

###################
### Secant and Newton-Raphson are for experts only
###################

# Secant method for solving the one-dimensional line search problem (inner iteration)
# Finds the root of the directional derivative of a multivariate function, but is not guaranteed to work
# Can be used for univariate functions as well, just set direction to 1
# WARNING: Method may not converge if initial values are not "near" the root -> in case of divergence return FALSE
sec_method <- function(start_val=NA,direction=NA,grad_f_name=NULL,accuracy=1e-6,max_iterations=10000){
  
  iter = 0
  # We need two initial values, because we replace the tangent by secant
  x_1 = 0
  x_2 = 1
  fx_1 = t(direction)%*%grad_f_name(start_val+x_1*direction)
  fx_2 = t(direction)%*%grad_f_name(start_val+x_2*direction)
  if (fx_1==fx_2) stop("Error in Secant method")
  sec_12 = (fx_1-fx_2)/(x_1-x_2)
  x_new = x_2 - fx_2/sec_12
  fx_new = t(direction)%*%grad_f_name(start_val+x_new*direction)
  
  # Stopping criterion, the norm of the gradient is bounded by a value in between 1e-12 and accuracy=1e-6 depending on the starting point
  stol = max(1e-12, accuracy * min(1,abs(fx_new))) # sqrt(sum(...)^2) is the 2-norm of whatever is inside the (...)
  # if the algorithm is not converging you can try different values
  # stol = 1e-6
  
  while (iter<max_iterations) {
    if (abs(fx_new) < stol | abs(x_2-x_new) < accuracy)  return(x_new)
    else {
      iter = iter + 1
      x_1 = x_2
      x_2 = x_new
      fx_1 = fx_2
      fx_2 = fx_new
      fx_test1 = t(direction)%*%grad_f_name(start_val+x_1*direction)
      fx_test2 = t(direction)%*%grad_f_name(start_val+x_2*direction)
      sec_12 = (fx_1-fx_2)/(x_1-x_2)
      x_new = x_2 - fx_2/sec_12
      fx_new = t(direction)%*%grad_f_name(start_val+x_new*direction)
    }
  }
  
  return(x_new)
  
} # end secant

# Newton-Raphson method for finding roots of the directional derivative of a multivariate function
# NEEDS HESSIAN so not applicable to all test functions
# WARNING: tol is critical, not easy to choose
nr_method <- function(start_val=NA,direction=NA,grad_f_name=NULL,hesse_f_name=NULL,accuracy=1e-6,max_iterations=10000){
  
  iter = 0
  x_old = 0
  x_new = 1
  fx_new = t(direction) %*% grad_f_name(start_val+x_new*direction)
  gfx_new = sum(t(direction) %*% hesse_f_name(start_val+x_new*direction) %*% direction)
  
  # Stopping criterion, the norm of the gradient is bounded by a value in between 1e-12 and accuracy=1e-6 depending on the starting point
  # tol = max(1e-12, accuracy * min(1,abs(gfx_new))) # sqrt(sum(...)^2) is the 2-norm of whatever is inside the (...)
  # if the algorithm is not converging you can try
  tol = accuracy
  
  while (iter<max_iterations) {
    if (abs(fx_new) < tol | abs(x_new-x_old) < accuracy) {
      return(x_new)
    }  else {
      iter = iter + 1
      x_old = x_new 
      fx_old = fx_new
      gfx_old = gfx_new
      x_new = x_old - (fx_old/gfx_old)
      fx_new = t(direction) %*% grad_f_name(start_val+x_new*direction)
      gfx_new = sum(t(direction)%*%hesse_f_name(start_val+x_new*direction)%*%direction)
    }
  }
  
  return(x_new)
  
} # end Newton-Raphson
