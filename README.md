# Optimization
Optimization Routines for codecentric blog articles

This repo contains a number of basic optimization routines. New routines are added as the blog series progresses. We apply these routines to different "toy" or test examples in order to illustrate their performance depending on the specific problem to solve. Finally - as a real-world application - we intend to train a Collaborative Filtering Recommender System, a methodology that will be introduced in the blog series as well. Furthermore, we show the performance for Linear Regression and Logistic Regression (as prototype of a Classification algorithm).

## Link to blog posts:
https://blog.codecentric.de/en/2016/01/machinery-linear-regression/

https://blog.codecentric.de/en/2015/04/machinery-part2/

https://blog.codecentric.de/en/2015/03/machinery-part1/

### Usage of R-Scripts for Blogpost 3

Important: Put all files in the same directory!

#### Main file:  Linear_Regression_test.R
Most important settings:
- number of data points
- number of parameters of the model
- stepsize method
- inital value

Just source the file.

You don't need to touch any other file, but the program depends on
- stepsizes.R
- grad_descent.R
- lin_reg_cost_grad.R

For more information see the comments in the respective files.

### Usage of R-Scripts for Blogpost 2

Important: Put all files in the same directory!

#### Main file:  Gradient_Descent_test.R
Most important settings:
- test function
- stepsize method
- problem dimension
- inital value

Just source the file.

You don't need to touch:
- stepsizes.R
- grad_descent.R
- test_functions.R
- grad_descent_minimal.R

If you want to experiment with different variants of the Rosenbrock function I recommend changing the parameters a and b directly in the function definition in the test_functions.R file. Please check that a and be are the same in both function and gradient. 

For learning/debugging it can be helpful to reduce the maximum number of iterations in grad_descent.R, you can change it directly in the function definition, the default value is max_iterations=1000000.

#### Advanced: Experiments with fixed stepsize

If you want to experience the effects of a fixed stepsize on your own, then try minimizing f_1,f_2 or f_3 first. Here is what you have to do: 
- In Gradient_Descent_test.R uncomment the line #stepsize = "fixed"
- In grad_descent.R check the value of the parameter fixed_stepsize, it should not be larger than 1e-3 in the beginning
- Run Gradient_Descent_test.R
- In case the algorithm reaches max_iterations before converging increase this value (in grad_descent.R)

When you feel ready for the Rosenbrock function f_4 then choose fixed_stepsize = 1e-7 first, in higher dimensions you have to take even smaller values. Don't forget to increase max_interations - and have a good book at hand for the waiting time ;-)

#### grad_descent_minimal.R

This is just to show you how compact Gradient Descent can be implemented. It is not integrated into the other scripts, but it should produce the same results. I have chosen Armijo with widening as stepsize, it is contained in the file as well so basically all you need to do is replacing grad_descent(...) by grad_descent_minimal(...) with an appropriate argument list.
