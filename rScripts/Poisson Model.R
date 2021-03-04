rm(list = ls())

# Before beginning, let's check what the Poisson and Gamma distributions look like
# PMF for Poisson:
pois<-function(x,rate){
  pdf = (1/factorial(x))*(rate^x)*(exp(-rate))
  return(pdf)
}

# recall that Poisson is a discrete distribution

xval = seq(0,10,by=1)
pmf1<-pois(x=xval,rate=2)
pmf2<-pois(x=xval,rate=3)
pmf3<-pois(x=xval,rate=4)
pmf4<-pois(x=xval,rate=5)
pmf5<-pois(x=xval,rate=6)
pmf6<-pois(x=xval,rate=7)

# as rate param increases, dist flattens and becomes centered at higher values of x
plot(xval,pmf1, type="b", col="red")
Sys.sleep(1)
points(xval,pmf2, type="b", col="blue")
Sys.sleep(1)
points(xval,pmf3, type="b", col="green")
Sys.sleep(1)
points(xval,pmf4, type="b", col="gold")
Sys.sleep(1)
points(xval,pmf5, type="b", col="brown")
Sys.sleep(1)
points(xval,pmf6, type="b", col="black")
Sys.sleep(1)

# gamma pdf:
gam<-function(x,shape,scale){
  pdf = ((scale^shape)/gamma(shape))*(x^(shape-1))*(exp(-scale*x))
  return(pdf)
}

# Keep scale the same and just vary shape 
xval = seq(from=0.01,to=10,by=0.01)
dens1<-gam(x=xval,shape=1,scale=1)
dens2<-gam(x=xval,shape=2,scale=1)
dens3<-gam(x=xval,shape=3,scale=1)
dens4<-gam(x=xval,shape=4,scale=1)
dens5<-gam(x=xval,shape=5,scale=1)
dens6<-gam(x=xval,shape=6,scale=1)

# plots
# as shape param increases, dist flattens and shifts out towards right
plot(xval,dens1, type="l", col="red")
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

# Keep shape the same and just vary scale
xval = seq(from=0.01,to=10,by=0.01)
dens1<-gam(x=xval,shape=2,scale=1)
dens2<-gam(x=xval,shape=2,scale=2)
dens3<-gam(x=xval,shape=2,scale=3)
dens4<-gam(x=xval,shape=2,scale=4)
dens5<-gam(x=xval,shape=2,scale=5)
dens6<-gam(x=xval,shape=2,scale=6)

# plots
# scrinches up the dist; upward shift with sharp peakening, thinner tails
plot(xval,dens1, type="l", col="red", ylim=c(0,1.6))
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

######################################################################################################################
# 2  Poisson Model:
#    Prior is gamma(alpha, beta)
#    Posterior is gamma(alpha + samp*ybar, beta + samp)

#    Simple Illustration:
#    Suppose we're labor economists, and we want to estimate the number of times workers receive on-the-job trainings
#    That is, we want to model the counts of on-the-job trainings.
#    Suppose we have data on the total number of on-the-job trainings throughout the entire careers of 30 workers.

# data
y = c(0,13,2,15,56,0,4,19,4,6,21,37,0,0,0,12,16,9,4,0,21,0,18,33,7,23,0,0,0,5)
ybar = mean(y)

# sample size
samp = length(y)

# Recall that we can think of the prior as contributing (alpha-1) counts in beta prior observations
# So that the prior doesn't overwhelm the data, let's set the "prior sample size" to be small
# --> small beta.
beta = 0.1
# The mean of a gamma distribution is alpha/beta and the variance of the gamma distribution is alpha/beta^2
# What might be a reasonably diffuse choice for alpha?
alpha = 1
# With alpha = 1, the prior mean = 10 and prior variance = 100
# prior mean = alpha/beta = 1/0.1 = 10
# Let's start with this and test sensitivity later.

# Simulate 1,000 times from the posterior
?rgamma
# Notice that base R parameterizes the scale parameter differently than how we have considered it in class.
# In base R, their scale parameter is the inverse of ours, so we will need to enter ours "inversely"
# ---> do 1/(beta + samp) instead of (beta + samp) for scale parameter.

post_pois=rgamma(1000,shape=alpha+samp*ybar,scale=1/(beta + samp))

# Posterior analysis 
# The analysis below seems reasonable 
hist(post_pois,breaks=50)
mean(post_pois)
# close to ybar
ybar
median(post_pois)
# The 95% credible interval
qgamma(c(0.025,0.975),shape=alpha+samp*ybar,scale=1/(beta + samp))
# interval contains ybar
# This seems like a reasonable analysis

# What would happen if we set alpha = 0.05 and beta = 0.01?
# Here, the prior mean = 5 and prior variance is 500, which seems like a better, more diffuse choice
# This is higher variance than before.
alpha = 0.05
beta = 0.01

post_pois=rgamma(1000,shape=alpha+samp*ybar,scale=1/(beta + samp))
hist(post_pois,breaks=50)
mean(post_pois)
median(post_pois)
# The 95% credible interval
qgamma(c(0.025,0.975),shape=alpha+samp*ybar,scale=1/(beta + samp))
# This again seems reasonable.

# Last, let's try alpha=0.009 and beta=0.0001
# This gives a very high prior mean and a somewhat large variance
# prior mean = 0.009/0.0001 = 90
# prior variance = 900,000 = 0.009/(0.0001)^2
alpha = 0.009
beta = 0.0001

post_pois=rgamma(1000,shape=alpha+samp*ybar,scale=1/(beta + samp))
hist(post_pois,breaks=50)
mean(post_pois)
median(post_pois)
# The 95% credible interval
qgamma(c(0.025,0.975),shape=alpha+samp*ybar,scale=1/(beta + samp))

# Once again, the posterior analysis doesn't seem to budge.
# The data overshadow the prior given that the real sample size is 30, but the "prior sample size" (beta) is less than one.