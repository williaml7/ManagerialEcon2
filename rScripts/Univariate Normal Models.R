rm(list = ls())

# Base R doesn't include a function to draw directly from a scaled inverse chi-square pdf
# We will use the geoR package for that
library(geoR)

#     Normal model with unknown mean and known variance (sig2):
#     Prior is normal(mu0, tau02)
#     Posterior is normal(mu_n, tau_n)
#       mu_n  = ((mu0/tau02)+(samp/sig2)*ybar)/((tau02^-1)+(samp/sig2))
#       tau_n = (sig2+samp*tau02)/(tau02*sig2)
#     Find these formulas from normal model with many observations.

##################################################################################################################
#     Simple illustration:
#     Suppose we have some data on changes in local government agency budgets between 2018 and 2019
#     Some agency's budgets went up, so they're positive; other agency's budgets went down, so they're negative
#     A new policymaker came into office then and wanted status quo. 
#     She/he announced a policy rule: interannual variability in agency budgets would be such that the standard deviation is $100.
#     This means sig2=(100)^2
#     Given this significant variability and the data, you want to find out the distribution for agency budgets 

# data
y = c(-100,23,-800,5,-500,60,-500,-200,13,10,120,-100,130,110,-200,35,-40,220,-189,-150)
ybar = mean(y)

# sample size
samp = length(y)
# known variance
sig2 = (100)^2

# hyperparameters for a weakly-informative prior: mean zero and variance of 1 million
mu0 = 0
tau02 = 1e6

# Simulate 1,000 times from the posterior 
mu_n = ((mu0/tau02)+(samp/sig2)*ybar)/((tau02^-1)+(samp/sig2))
tau_n = (tau02*sig2)/(sig2+samp*tau02)

post_norm_mu = rnorm(1000, mean=mu_n, sd=sqrt(tau_n))

# Posterior analysis 
# The analysis below seems reasonable 
hist(post_norm_mu,breaks=50)
mean(post_norm_mu)
# seems close to ybar
ybar
median(post_norm_mu)
# The 95% credible interval
qnorm(c(0.025,0.975),mean=mu_n, sd=sqrt(tau_n))
quantile(post_norm_mu, c(0.025, 0.975))
# we can that the 95% credible interval includes ybar

# Some sensitivity analysis on the above

# What happens if we use a more informative prior? Suppose we think that the true mean is zero, and we are confident in this.
# Suppose we set the prior variance to be quite small, say 100
# Express greater confidence in prior belief by setting low prior variance!

tau02 = 100
mu_n = ((mu0/tau02)+(samp/sig2)*ybar)/((tau02^-1)+(samp/sig2))
tau_n = (tau02*sig2)/(sig2+samp*tau02)

post_norm_mu = rnorm(1000,mean=mu_n,sd=sqrt(tau_n))

# Posterior analysis 
# This analysis seems unreasonable. 
# The posterior mean is centered an order of magnitude away from the sample mean.
# The 95% posterior interval doesn't contain the mean of the data
hist(post_norm_mu,breaks=30)
mean(post_norm_mu)
median(post_norm_mu)
# The 95% credible interval
qnorm(c(0.025,0.975),mean=mu_n,sd=sqrt(tau_n))
quantile(post_norm_mu, c(0.025, 0.975))
# Notice how the interval doesn't even include the sample mean!
ybar

# What happens with an even more diffuse prior? Suppose we set the prior variance to 100 trillion.
# This represents a very weak prior belief; data will speak loudly.

tau02 = 1e14
mu_n = ((mu0/tau02)+(samp/sig2)*ybar)/((tau02^-1)+(samp/sig2))
tau_n = (tau02*sig2)/(sig2+samp*tau02)

post_norm_mu=rnorm(1000,mean=mu_n,sd=sqrt(tau_n))

hist(post_norm_mu,breaks=50)
mean(post_norm_mu)
median(post_norm_mu)
qnorm(c(0.025,0.975),mean=mu_n,sd=sqrt(tau_n))
quantile(post_norm_mu, c(0.025, 0.975))
# The posterior analysis is virtually unchanged from the first example.

# Before beginning the normal model with unknown variance:
# Let's see what a scaled inverse chi-square distribution looks like
# First, let's double check that geoR's pdf matches up with the pdf we've been using in lecture
?rinvchisq

# Try out a few values of the degrees of freedom and scale parameters
scinvchsq<-function(x,df,scale){
  
  pdf = (((df/2)^(df/2))/(gamma(df/2)))*(scale^df)*((1/x)^((df/2)+1))*exp(-(df*(scale^2))/(2*x))
  
  return(pdf)
}

# Let's hold the scale parameter fixed at one and see what happen as df increases
# as df param increases, dist becomes more peaked and centralized about higher values of x
xval = seq(from=0.01,to=5,by=0.01)
dens1<-scinvchsq(x=xval,df=1,scale=1)
dens2<-scinvchsq(x=xval,df=2,scale=1)
dens3<-scinvchsq(x=xval,df=3,scale=1)
dens4<-scinvchsq(x=xval,df=4,scale=1)
dens5<-scinvchsq(x=xval,df=5,scale=1)
dens6<-scinvchsq(x=xval,df=6,scale=1)

# plots
plot(xval,dens1, type="l", col="red",ylim=c(0,0.8))
Sys.sleep(1)
points(xval,dens2, type="l", col="blue")
Sys.sleep(1)
points(xval,dens3, type="l", col="green")
Sys.sleep(1)
points(xval,dens4, type="l", col="gold")
Sys.sleep(1)
points(xval,dens5, type="l", col="brown")
Sys.sleep(1)
points(xval,dens6, type="l", col="black")
Sys.sleep(1)

# Now let's hold the df fixed at one and see what happens as the scale increases
# as scale param increases, dist becomes flatter and has thicker tails (i.e., higher variance).
dens7<-scinvchsq(x=xval,df=1,scale=1)
dens8<-scinvchsq(x=xval,df=1,scale=1.1)
dens9<-scinvchsq(x=xval,df=1,scale=1.2)
dens10<-scinvchsq(x=xval,df=1,scale=1.3)
dens11<-scinvchsq(x=xval,df=1,scale=1.4)
dens12<-scinvchsq(x=xval,df=1,scale=1.5)

# plots
plot(xval,dens7, type="l", col="red",ylim=c(0,0.8))
Sys.sleep(1)
points(xval,dens8, type="l", col="blue")
Sys.sleep(1)
points(xval,dens9, type="l", col="green")
Sys.sleep(1)
points(xval,dens10, type="l", col="gold")
Sys.sleep(1)
points(xval,dens11, type="l", col="brown")
Sys.sleep(1)
points(xval,dens12, type="l", col="black")
Sys.sleep(1)

###############################################################################################################################
#     Normal model with known mean (theta) unknown variance:
#     Prior is scaled inverse chi squared(nu0, sig02)
#     Posterior is scaled inverse chi squared (df2, scale2)
#       df2     = nu0 + samp
#       scale2  = (nu0*sig02+samp*v)/(nu0+samp)
#       v       = (1/samp)*sum((y-theta)^2)

#     Simple illustration:
#     Instead, suppose we have a slightly different policymaker.
#     She/he announced a plan to "reduce the size" of local government through a reduction of each agency's budget.
#     In this case, the known mean is -100,000. We want to find out how much variability in the data this policy induces.
#     Suppose we have the following data.

# data
y = c(-100000,-225000,-2000,1000,-38000,-69000,230,-87000,2801,35000,-119000,-89000,-54000,-138000,-325000,-400000)
ybar = mean(y)
var(y)

# sample size
samp = length(y)
# known mean
theta = -100000
# for the prior, let's start with something relatively diffuse. Check what the mean and variance is if
# prior df parameter:
nu0 = 2 # --> E(theta) = (2/(2-2))*15^2 = Inf; this is why ppl specify df>2 usually
# prior scale parameter:
sig02 = 15

# Simulate 1,000 times from the posterior 
# posterior df:
df2     = nu0 + samp
# v
v       = (1/samp)*sum((y-theta)^2)
# posterior scale:
scale2  = (nu0*sig02+samp*v)/(nu0+samp)

post_norm_var=rinvchisq(1000, df=df2, scale=scale2)

# Posterior analysis 
# The analysis below seems reasonable 
hist(post_norm_var,breaks=50)

# These are large numbers, so it's hard to get a sense of how far off they are from the data.
# Express the mean of the posterior variance as a fraction of the sample variance
(mean(post_norm_var)-var(y))/var(y)
(median(post_norm_var)-var(y))/var(y)

mean(post_norm_var)
median(post_norm_var)
var(y)

# The 95% credible interval
quantile(post_norm_var, c(0.025,0.975))
# Note that this includes our sample variance
var(y)
