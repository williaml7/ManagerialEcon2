rm(list = ls())

# Let's try a simple Bayesian analysis
# Likelihood: Binomial, Prior: Uniform(0,1)

# For the sake of example, we will make up some data:
# n = 100
# y = 40

# Posterior =  beta(y+1,n-y+1)
# Note that statisticians sometimes have different expressions for the same pdf
# We need to double check that R writes the pdf the same way as the textbook (see pages 580-581)
?rbeta

# Generate 1,000 draws from the posterior beta distribution
postdraws = rbeta(1000,shape1=40+1,shape2=100-40+1)

# Summarize the draws by looking at the histogram and different percentiles of the draws
hist(postdraws,breaks=50)
summary(postdraws)

# Posterior mean = (y+1)/(n+2)
# This is a comprise between the prior mean (1/2) and the sample proportion (y/n)

# Example 2
n = 100
y = 10

# Notice that the posterior "tracks" the data closely, even though the sample size is only 100 births
# Prior mean = 0.5, sample proportion = 0.1, but the posterior mean is very close to 0.5
postdraws2 = rbeta(1000,shape1=10+1,shape2=100-10+1)
hist(postdraws2,breaks=50)
summary(postdraws2)

# Example 3
n = 100
y = 90

# Again, we see that the posterior draws tracking the data closely
postdraws3 = rbeta(1000,shape1=90+1,shape2=100-90+1)
hist(postdraws3,breaks=50)
summary(postdraws3)

# Example 4
# What happens when the sample size is very small?
# The posterior has much greater spread and posterior mean is more influenced by prior mean
# Prior mean = 0.5, sample proportion = 0.4, posterior mean = (y+1)/(n+2) = (3/7)
n = 5
y = 2

postdraws4 = rbeta(1000,shape1=2+1,shape2=5-2+1)
hist(postdraws4,breaks=50)
summary(postdraws4)

# Example 5
# What happens when the sample size is very large?
# The posterior has very little spread and the posterior mean is centered tightly around the sample proportion
# Prior mean = 0.5, sample proportion = 0.95, posterior mean = (y+1)/(n+2) = 9501/10002 = 0.9499
n = 10000
y = 9500

postdraws5 = rbeta(1000,shape1=9500+1,shape2=10000-9500+1)
hist(postdraws5,breaks=50)
summary(postdraws5)