# ECON 5043: Homework 2 R Script (2/21/21)
# William Lorton
library(tidyverse)

# Problem 2

  # Calculating some binomial coefficients
  # ways to choose k (y in our problem) items from 10 without regard to order
  # formula: (n! / [k!(n-k)!])
  choose(n = 10, k = 0)
  choose(10, 1)
  choose(10, 2)

# Problem 5
  
  # Data is from PSID 1990 wave
  # Focus on ftemp variable (1 --> full employment status)
  # Assume y|theta ~ Binomial(4689, theta); 
  # theta is P(full employment status) for a given individual
  
  # read data in
  employment <- read.csv("Employment.csv")
  
  # mean of ftemp variable
  ftemp.mean <- mean(employment$ftemp)
  ftemp.mean
  
  # median of ftemp
  ftemp.median <- median(employment$ftemp)
  ftemp.median
  
  # standard deviation of ftemp
  ftemp.sd <- sd(employment$ftemp)
  ftemp.sd
  
  # number of people fully employed in sample
  employment %>% 
    group_by(ftemp) %>% 
    count()
  # 4210 people fully employed
  
  # Uniform (0,1) prior for theta
  # Posterior is theta|y ~ Beta(y+1, n-y+1)
  # where n = 4689, y = 4210
  n <- 4689 # number of people in sample
  y <- 4210 # number of people with full time employment
  # Want 10,000 random draws from this dist
  post_draws_unif <- rbeta(10000, shape1 = y + 1, shape2 = n - y + 1)
  
  # histogram of draws
  hist(post_draws_unif, breaks = 50)
    
  # mean and median of draws
  mean(post_draws_unif)
  median(post_draws_unif)
  # alternate median
  qbeta(0.50, shape1 = y + 1, shape2 = n - y + 1)
  
  # 95% credible interval for theta
  qbeta(c(0.025, 0.975), shape1 = y + 1, shape2 = n - y + 1)
  
  # Now we assume a Beta(alpha, beta) prior for theta
  # where alpha = 1.5, beta = 0.5
  alpha <- 1.5
  beta <- 0.5
  
  # Get 10,000 random draws from this beta prior
  prior_draws_beta <- rbeta(10000, shape1 = alpha, shape2 = beta)
  
  # histogram of beta prior draws
  hist(prior_draws_beta, breaks = 50)
  
  # Binomial likelihood and beta prior -->
  # Beta(alpha+y, beta+n-y) posterior
  
  # Getting 10k draws from this posterior dist
  post_draws_beta <- rbeta(10000, shape1 = alpha + y, shape2 = beta + n - y)

  # histogram of beta posterior draws
  hist(post_draws_beta, breaks = 50)
  
  # mean and median of beta posterior draws
  mean(post_draws_beta)
  median(post_draws_beta)
  # alternate median
  qbeta(0.50, shape1 = alpha + y, shape2 = beta + n - y)
  
  # 95% credible interval for this posterior
  qbeta(c(0.025, 0.975), shape1 = alpha + y, shape2 = beta + n - y)
  