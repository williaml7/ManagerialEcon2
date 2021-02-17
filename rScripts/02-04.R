rm(list = ls())

# Log transformations
x = seq(from = 0, to = 1000, by = 0.01)
logx = log(x)
plot(x, logx, type = "l")

# Logit transformation
u = seq(from = 0, to = 1, by = 0.0001)
logitu = log(u/(1-u))
plot(u, logitu, type = "l")

# Probit transformation
# By definition of the quantile function, qnorm provides the inverse of the normal CDF
# pnorm gives regular normal cdf
plot(u, qnorm(u, mean = 0, sd = 1), type = "l")

# Compare inverse CDF method of sampling to direct sampling
# We will sample from the Exponential distribution, as in lecture
# Suppose rate = 5 (value of lambda)
rate = 5

# Set the seed for comparability 
set.seed(1)

# Get random draws from a U(0,1) distribution
U = runif(n = 10000, min = 0, max = 1)
# inverse exponential cdf to sample from; histogram looks like exponential distribution
# insert U inside
v1 = -log(U)/rate

# now directly from the exponential distribution with same rate parameter
set.seed(1)
v2 = rexp(n = 10000, rate = rate)

# Look at histograms
hist(v1, breaks = 50)
hist(v2, breaks = 50)

# We can also convince ourselves that U and 1-U have essentially the same distribution
hist(U, breaks = 50)
hist(1-U, breaks = 50)


# Binomial Model Example

# For the sake of example, we will make up some data:
# sample of 100 births
n = 100
# 40 births are female out of 100
y = 40

# Posterior =  beta(y+1,n-y+1); we solved for this
# shape parameter alpha is y+1 and shape parameter beta is n-y+1

# Note that statisticians sometimes have different expressions for the same pdf
# We need to double check that R writes the pdf the same way as the textbook (see pages 580-581)
?rbeta

# Generate 1,000 draws from the posterior beta distribution
postdraws = rbeta(1000,shape1=y+1,shape2=n-y+1)

# Summarize the draws by looking at the histogram and different percentiles of the draws
hist(postdraws,breaks=50)
# Infer that the true probability of a female birth is around the mean given in the summary output
summary(postdraws)

# Posterior mean = (y+1)/(n+2)
# This is a comprise between the prior mean (1/2) and the sample proportion (y/n)

# Example 2
n = 100
y = 10

# Notice that the posterior "tracks" the data closely, even though the sample size is only 100 births
# Prior mean = 0.5, sample proportion = 0.1, and the posterior mean is very close to 0.1
postdraws2 = rbeta(1000,shape1=y+1,shape2=n-y+1)
hist(postdraws2,breaks=50)
summary(postdraws2)

# Example 3
n = 100
y = 90

# Again, we see that the posterior draws tracking the data closely
postdraws3 = rbeta(1000,shape1=n+1,shape2=n-y+1)
hist(postdraws3,breaks=50)
summary(postdraws3)

# Example 4
# What happens when the sample size is very small?
# The posterior has much greater spread and posterior mean is more influenced by prior mean
# Prior mean = 0.5, sample proportion = 0.4, posterior mean = (y+1)/(n+2) = (3/7)
n = 5
y = 2

postdraws4 = rbeta(1000,shape1=y+1,shape2=n-y+1)
hist(postdraws4,breaks=50)
summary(postdraws4)

# Example 5
# What happens when the sample size is very large?
# The posterior has very little spread and the posterior mean is centered tightly around the sample proportion
# Prior mean = 0.5, sample proportion = 0.95, posterior mean = (y+1)/(n+2) = 9501/10002 = 0.9499
n = 10000
y = 9500

postdraws5 = rbeta(1000,shape1=y+1,shape2=n-y+1)
hist(postdraws5,breaks=50)
summary(postdraws5)