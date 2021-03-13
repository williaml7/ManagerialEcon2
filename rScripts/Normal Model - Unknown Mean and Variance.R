rm(list = ls())

# load the geoR package 
library(geoR)

# Suppose we have some data on weekly income from a second job
y=c(50, 75, 235, 129, 77, 39, 41, 329, 56, 23, 119, 178, 68, 149, 210, 107, 64, 89, 275, 92)

# Part (a)
ybar=mean(y)
s_sq = var(y)

# Define sample size, degrees of freedom, and total number of draws
samp=length(y)
df2 = samp-1
total<-500000

# Create placeholder vectors for the posterior variance, posterior mean, and posterior predictions
post_norm_var<-rep(0,total)
post_norm_mu<-rep(0,total)

# Loop
# Step 1: get a draw for the variance
# Step 2: get a draw for the mean, conditional on what you just drew in (1) for the variance
# Repeat the steps, in this example, 1000 times 
for (i in 1:total){
  post_norm_var[i]=rinvchisq(n=1, df2, scale=s_sq)
  #Sys.sleep(1)
  #print("Variance draw is")
  #print(post_norm_var[i])
  post_norm_mu[i]=rnorm(n=1,mean=ybar,sd=sqrt(post_norm_var[i]/samp))
  #Sys.sleep(1)
  #print("Mean draw is")
  #print(post_norm_mu[i])
}

# Histograms
hist(post_norm_mu,breaks=50)
hist(post_norm_var,breaks=50)

# Compare the means and medians with the data 
mean(post_norm_mu)
median(post_norm_mu)
ybar 
mean(post_norm_var)
median(post_norm_var)
s_sq

# 95% Credible intervals
quantile(post_norm_mu,c(0.025,0.975))
quantile(post_norm_var,c(0.025,0.975))

# 95% confidence intervals?
lower<-ybar-(qt(p=.05/2,df=df2,lower.tail=FALSE)*sqrt(s_sq)/sqrt(samp))
upper<-ybar+(qt(p=.05/2,df=df2,lower.tail=FALSE)*sqrt(s_sq)/sqrt(samp))
lower
upper