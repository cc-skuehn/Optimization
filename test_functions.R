# test functions and gradients (hessians in one case)

# Function f_1: 0.5 * squared euclidean norm of x = 0.5* (x_1^2+ ... + x_n^2), simple radial function
f_test = function(x){
  count_f <<- count_f + 1
  return(0.5*sum(x^2))
}

grad_test = function(x){
  count_g <<- count_g + 1
  return(x)
}

hesse_test = function(x){
  count_h <<- count_h + 1
  lx = length(x)
  H = matrix(0,nrow=lx,ncol=lx)
  for (i in 1:lx) H[i,i] = 1 # identity matrix
  return(H)
}

# end f_1

# Function f_2: 0.5 * (1^2*x_1^2 + 2^2*x_2^2 + ... + i^2*x_i^2 + ... + n^2*x_n^2), simple anisotropic, nonsymmetric function
aniso_test = function(x){
  count_f <<- count_f + 1
  return(0.5*sum(((1:length(x))*x)^2))
}

aniso_grad_test = function(x){
  count_g <<- count_g + 1
  return((1:length(x))^2*x)
}

aniso_hesse_test = function(x){
  count_h <<- count_h + 1
  lx = length(x)
  H = matrix(0,nrow=lx,ncol=lx)
  for (i in 1:lx) H[i,i] = i^2
  return(H)
}
# end f_2

# Function f_3: sum(x^4) = x_1^4 + ...+ x_n^4, another simple symmetric function
sq_test = function(x){
  count_f <<- count_f + 1
  return(sum(x^4))
}

sq_grad_test = function(x){
  count_g <<- count_g + 1
  return(4*x^3)
}

sq_hesse_test = function(x){
  count_h <<- count_h + 1
  lx = length(x)
  H = matrix(0,nrow=lx,ncol=lx)
  for (i in 1:lx) H[i,i] = 12*x[i]^2
  return(H)
}

# end f_3

# Function f_4: Rosenbrock test function, advanced version
# Attention: When you change a and b: do it for both functions
rosenbrock_advanced = function(x,a=1,b=100){
#rosenbrock_advanced = function(x,a=1,b=5){
  count_f <<- count_f + 1
  lx = length(x)
  f_val = 0
  for (i in 1:(lx-1)) {
     f_val = f_val + (a-x[i])^2+b*(x[i+1]-x[i]^2)^2
  }
  return(f_val)
}

grad_rosenbrock_advanced =  function(x,a=1,b=100){
#grad_rosenbrock_advanced =  function(x,a=1,b=5){
  count_g <<- count_g + 1
  lx = length(x)
  if (lx<2) stop('Not multivariate!')
  grad_val = rep(0,lx)
  grad_val[1] = -2*(a-x[1]) - 4*b*x[1]*(x[2]-x[1]^2)
  if (lx>2) {    
    for (i in 2:(lx-1)) {
      grad_val[i] = grad_val[i] - 2*(a-x[i]) - 4*b*x[i]*(x[i+1]-x[i]^2) + 2*b*(x[i]-x[i-1]^2)
    }
  }
  grad_val[lx] = 2*b*(x[lx]-x[lx-1]^2)
  return(grad_val)
}
# end f_4
