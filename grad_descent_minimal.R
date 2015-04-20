# Minmal Implementation of Gradient Descent using Armijo with widening - Basics

grad_descent_minimal <- function(start_val=NA, f_name=NULL, grad_f_name=NULL, accuracy=1e-6, max_iterations=1000000){
  
  # Set/compute initial values
  x = start_val # vector in R^n 
  gfx = grad_f_name(x) # gradient at x, vector in R^n
  iter = 0 # iteration counter
  # Start loop, check stopping/running conditions, stops if norm(gradient) <= accuracy,  Armijo with widening
  while (sqrt(sum(gfx^2)) > accuracy & iter < max_iterations) {
    t = armijo_wide_minimal(x_val=x,grad_x_val=gfx,direction=-gfx,f_arm=f_name)
    x = x - t * gfx
    gfx = grad_f_name(x)
    iter = iter + 1
  } # end while loop
  count_iter <<- iter

  return(x)
}

# Armijo rule with widening, line search
# sig: "Relaxation" parameter (sigma), sig < 0.5
# al: Stepsize parameter (alpha), al < 1.0
armijo_wide_minimal = function(sig=0.1, al=0.5, x_val=NA, grad_x_val=NA, direction=NA, f_arm=NULL){
  
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