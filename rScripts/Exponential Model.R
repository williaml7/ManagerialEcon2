rm(list = ls())

#####################################################################################################################################
#    Exponential Model:
#    Prior is gamma(alpha, beta)
#    Posterior is gamma(alpha + samp, beta + samp*ybar)

#    Simple Illustration:
#    Again we're labor economists, and we want to study the length of duration of unemployment.
#    Suppose we are fine with the "memoryless" property in this case. We think it's not unreasonable that the probability a worker will be unemployed
#    a little longer than he/she currently is is independent of how long thus far she/he has been unemployed.
#    Suppose we have data on the number of consecutive months that 10 workers have been unemployed.

y = c(2,3,6,1,2,1,1,2,3,7)
ybar = mean(y)

# sample size
samp = length(y)

# Try this initial prior
# "prior sample size" of 55-1 = 54!
alpha = 55
beta = 0.01

# invert the scale parameter since R writes the gamma dist differently than our textbook 
post_exp=rgamma(1000,shape=alpha+samp,scale=1/(samp*ybar+beta))

# Posterior analysis 
# Given the form of the posterior, the analysis here is more challenging 
hist(post_exp,breaks=50)
mean(post_exp)
median(post_exp)
# The 95% credible interval
qgamma(c(0.025,0.975),shape=alpha+samp,scale=1/(samp*ybar+beta))

