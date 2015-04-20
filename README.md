# Optimization
Optimization Routines for codecentric blog articles

This repo contains a number of basic optimization routines. New routines are added as the blog series progresses. We apply these routines to different "toy" or test examples in order to illustrate their performance depending on the specific problem to solve. Finally - as a real-world application - we intend to train a Collaborative Filtering Recommender System, a methodology that will be introduced in the blog series as well.

## Link to blog posts:
https://blog.codecentric.de/en/2015/03/machinery-part1/

### Second blog post to appear on April 21st at 07:07 CET
https://blog.codecentric.de/en/2015/04/machinery-part2/

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

If you want to experiment with different variants of the Rosenbrock function I recommend changing the parameters a and b directly in the function definition in the test_functions.R file. Please check that a and be are the same in both function and gradient. 

For learning/debugging it can be helfup to reduce the maximum number of iterations in grad_descent.R, you can change the number directly in the function definition, the default value is max_iterations=1000000.
