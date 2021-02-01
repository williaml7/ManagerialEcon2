rm(list = ls())

# Install the 'visualize' package first
library(visualize)

# Uniform (0,1)
visualize.unif(a = 0, b = 1)

# Standard normal: mean = 0, sd = 1
visualize.norm(mu = 0, sd = 1)

# Normal: mean = 100 and standard deviation = 100
visualize.norm(stat = 100, mu = 100, sd = 20)

# Chi-Square with degrees of freedom parameter = 5
visualize.chisq(stat = 1, df = 5)

# Exponential with rate parameter = 3
visualize.exp(stat = 1, theta = 3)

# Beta (1,1), which is exactly the same as Uniform (0,1)
visualize.beta(stat = 1, alpha = 1, beta = 1)

# Beta: try a few different values of alpha and beta parameters
visualize.beta(stat = 1, alpha = 0.5, beta = 0.5)
visualize.beta(stat = 1, alpha = 1, beta = 2)
visualize.beta(stat = 1, alpha = 2, beta = 1)
visualize.beta(stat = 1, alpha = 2, beta = 2)
visualize.beta(stat = 1, alpha = 2, beta = 3)
visualize.beta(stat = 1, alpha = 1, beta = 3)
visualize.beta(stat = 1, alpha = 3, beta = 3)

# Gamma with alpha = 1 and beta = 1/2
visualize.gamma(stat = 1, alpha = 1, theta = 2)

# Gamma: try a few different values of alpha and beta parameters
visualize.gamma(stat = 1, alpha = 2, theta = 2)
visualize.gamma(stat = 1, alpha = 3, theta = 2)
visualize.gamma(stat = 1, alpha = 4, theta = 2)
visualize.gamma(stat = 1, alpha = 2, theta = 4)
visualize.gamma(stat = 1, alpha = 1, theta = 1)
visualize.gamma(stat = 1, alpha = 1, theta = 0.1)

# Binomial with size = 10 and probability of success = 1/2
visualize.binom(stat = 5, size = 10, prob = 0.5)

# Poisson with rate parameter = 10
visualize.pois(stat = 5, lambda = 10)