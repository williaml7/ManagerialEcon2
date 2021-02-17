rm(list = ls())

# Suppose we are analyzing birth data from a sample of individuals with placenta previa 
# Early German data suggest 437 female births from a total of 980 births. So y=437,n=980,n-y=543

# Model: Binomial likelihood and (Conjugate) Uniform (0,1) Prior -> Beta Posterior 
# Posterior: Beta(y+1,n-y+1) = Beta(438,544)
# Posterior mean = 438/(438+544) = 438/982 (expected value of beta dist)
# Following standard Bayesian practices, we will simulate from this posterior

# Draw 1,000 times independently from the Beta(438,544 posterior)
post_draws_un<-rbeta(n=1000,shape1=438,shape2=544)

# Use histogram of the posterior draws to characterize inference on theta, the true sex ratio parameter
hist(post_draws_un,breaks=50)

# The 95% credible interval is just the 2.5th and 97.5th percentile of this beta distribution
# Notice that the prior mean, 0.5, is not included in the 95th posterior interval 
qbeta(c(0.025,0.975),shape1=438,shape2=544)

# Normal approximation to the 95% credible interval
# This is just the posterior mean plus/minus z score times posterior standard deviation 
norm_approx<-c(0.445-1.96*0.016,0.445+1.96*0.016)
norm_approx

# Examine the median and first two moments of the draws
# Check that they are comparable to the exact mean and standard deviation
# For a Beta(438,544) distribution, E(theta|y)=0.446 and sqrt(var(theta|y))=0.016.
qbeta(0.50,shape1=438,shape2=544)
mean(post_draws_un)
sd(post_draws_un)

# Suppose we want to approximate the ratio with a normal distribution
# A first step in this would be to apply the logit transformation, i.e., log(theta/(1-theta))
# Let's do a Bayesian analysis of the transformed parameter

logit_theta<-log(post_draws_un/(1-post_draws_un))

# Histogram of the transformed draws
hist(logit_theta,breaks=50)

# Median, mean, and std dev of the transformed draws
median(logit_theta)
mean(logit_theta)
sd(logit_theta)

# Suppose we were interested in the sex ratio, i.e., the probability of male to female births
# Sex ratio = (1-theta)/theta

sex_ratio<-(1-post_draws_un)/post_draws_un

# Histogram of the sex ratio
hist(sex_ratio,breaks=50)

# Median, mean, and std dev of the sex ratio
median(sex_ratio)
mean(sex_ratio)
sd(sex_ratio)

# The 95% credible interval for the sex ratio is the 2.5th and 97.5th percentile of the draws
# Note that we're looking at a 95% interval for a FUNCTION of the parameter of interest!
# This is not as easy to do in classical statistics!
quantile(sex_ratio,c(0.025,0.975))

# Sensitivity Analysis: How does inference change when prior beliefs change; Distribution change? Hyperparameter change?
# Suppose we wanted to use the beta prior instead of the uniform prior.
# Posterior: Beta(alpha+y,beta+n-y)
# How would this change our analysis? 
# Under the Uniform[0,1] distribution, we know that E[U]=0.5, Var[U]=1/12. You can check that Beta(1,1) is just U[0,1]
# Thus, nothing would change going from uniform[0, 1] to beta(1,1)

# What if our prior was that the probability of female birth given p.p. was 1/3, or alpha=1, beta=2?
# Posterior: Beta(1+437,2+980-437) = Beta(438,545)
# Rest assured that your inference from Beta(438,545) will be close to that from Beta(438,544)
# Why? Because the data greatly overshadow the effect of the hyperparameters.

# New set of draws
post_draws_bet1<-rbeta(n=1000,shape1=438,shape2=545)

# Histogram, 95% posterior interval, median, mean, and std dev
hist(post_draws_bet1,breaks=50)
qbeta(c(0.025,0.975),shape1=438,shape2=545)
qbeta(0.50,shape1=438,shape2=545)
mean(post_draws_bet1)
sd(post_draws_bet1)

# On the other hand, what if our prior probability of female birth given p.p. was near zero? Say alpha = 0.01 and beta=0.99? 
# --> mean of our beta prior density is (0.01) / (0.01 + 0.99) = 0.01 [very low chance of fem birth]
# Posterior: Beta(0.01+437,0.99+980-437) = Beta(437.01,543.99)
# Rest assured that your inference from Beta(437.01,543.99) will be close to that from Beta(438,544)
# Why? Again, the data speak 'loudly' here.

# New set of draws
post_draws_bet2<-rbeta(n=1000,shape1=437.01,shape2=543.99)

# Histogram, 95% posterior interval, median, mean, and std dev
hist(post_draws_bet2,breaks=50)
qbeta(c(0.025,0.975),shape1=437.01,shape2=543.99)
qbeta(0.50,shape1=437.01,shape2=543.99)
mean(post_draws_bet2)
sd(post_draws_bet2)

# What if we had a bigger "prior" sample size? --> larger shape parameters --> more weight on prior belief.
# With (alpha,beta)=(500,500), it's 'as if' we have information from 1,000 prior births.
# Note that the expected value is (500/1000) = 1/2.
# Posterior: Beta(alpha+y,beta+n-y) = Beta(500+437,500+980-437)=Beta(937,1043)
# Results are slightly less robust. If you round up, the 95% interval 

# New set of draws
post_draws_bet3<-rbeta(n=1000,shape1=937,shape2=1043)

# Histogram, 95% posterior interval, median, mean, and std dev
hist(post_draws_bet3,breaks=50)
qbeta(c(0.025,0.975),shape1=937,shape2=1043)
qbeta(0.50,shape1=937,shape2=1043)
mean(post_draws_bet3)
sd(post_draws_bet3)

# Now what if we had a big "prior" sample size with a prior that female births given pp are virtually non-existent?
# Suppose alpha = 0.01, beta = 2000 --> prior mean is (0.01) / (0.01 + 2000) = [very small number]
# Posterior: Beta(alpha+y,beta+n-y) = Beta(0.01+437,2000+980-437) = Beta(437.01,2543)
# The inference below is now much more influenced by the prior, given that the "prior sample size" is much larger than the actual sample
# The 95% posterior interval doesn't contain y/n = 437/980 = 0.446.
# In real life, though, it would be difficult to justify this prior given the data, expectations of female birth, and knowledge of p.p.

# New set of draws
post_draws_bet4<-rbeta(n=1000,shape1=437.01,shape2=2543)

# Histogram, 95% posterior interval, median, mean, and std dev
hist(post_draws_bet4,breaks=50)
qbeta(c(0.025,0.975),shape1=437.01,shape2=2543)
qbeta(0.50,shape1=437.01,shape2=2543)
mean(post_draws_bet4)
sd(post_draws_bet4)

# Notice that you would get a similar but "opposite" outcome if your prior mean that female births given pp were virtually guaranteed
# Set alpha = 2000, beta = 0.01 --> prior mean is (2000) / (2000 + 0.01) = [close to 1]
# Posterior: Beta(alpha+y,beta+n-y) = Beta(2000+437,0.01+980-437) = Beta(2437,543.01)

# New set of draws
post_draws_bet5<-rbeta(n=1000,shape1=2437,shape2=543.01)

# Histogram, 95% posterior interval, median, mean, and std dev
hist(post_draws_bet5,breaks=50)
qbeta(c(0.025,0.975),shape1=2437,shape2=543.01)
qbeta(0.50,shape1=2437,shape2=543.01)
mean(post_draws_bet5)
sd(post_draws_bet5)