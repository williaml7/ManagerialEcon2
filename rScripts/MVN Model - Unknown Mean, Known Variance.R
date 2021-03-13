rm(list = ls())
library(MASS)
library(ggplot2)
library(matrixcalc)

setwd("C:/Users/admin/Downloads")

# Data are most recent available average electricity prices from EIA 
# Prices are average cents/kWh per customer type 
# In this example, k = 3 (residential, commercial, industrial)
data<-read.csv("ElectricityPrices.csv",header=T)

# Mean of 3 data vectors 
ybars = as.matrix(apply(data[,3:5],2,mean),nrow=3,ncol=1)

# Sample size (just use the row dimension of the data matrix)
n = dim(data)[1]

# Assume we know the true 3x3 covariance matrix
# For simplicity, we will all variances = 25 and covariances = 20
sig = matrix(c(25,20,20,20,25,20,20,20,25),nrow=3,ncol=3,byrow=T)

# Hyperparameters for a weakly-informative prior
# We know there should generally be a positive correlation between any two prices
# Let's set large variance terms (1e6) covariance terms (1e7)
m = rbind(0,0,0)
gamma = matrix(c(1e6,1e7,1e7,1e7,1e6,1e7,1e7,1e7,1e6),nrow=3,ncol=3,byrow=T)

# Posterior mean and posterior variance
mu_n = solve(solve(gamma) + n*solve(sig))%*%(solve(gamma)%*%m + n*solve(sig)%*%ybars)
tau_n = solve(solve(gamma)+n*solve(sig))

# Get 1,000 draws from the multivariate normal distribution
?mvrnorm
draws<-mvrnorm(n=1000,mu=mu_n,Sigma=tau_n)
head(draws)

# Define data matrices for plotting histograms simultaneously
draws<-data.frame(vec(draws))
drawscust<-as.matrix(c(rep("Res",1000),rep("Comm",1000),rep("Ind",1000)))
drawsmat<-as.data.frame(cbind(draws,drawscust))
colnames(drawsmat)<-c("Price","Customer")

# Histograms of the posterior means 
plot<-ggplot(drawsmat, aes(Price, fill = Customer, color = Customer)) + geom_histogram(alpha = 0.40, aes(y = ..density..), position = 'identity', bins=40) + geom_density(alpha=0,size=1.1) + labs(title="Histogram of Posterior Means")
plot<-plot+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_blank(), axis.title.y = element_blank())
plot

# The marginal posteriors are just one-variable normal distributions
# We can analyze the 95% credible intervals for the individual posterior means
qnorm(c(0.025,0.975),mean=mu_n[1],sd=sqrt(tau_n[1,1]))
qnorm(c(0.025,0.975),mean=mu_n[2],sd=sqrt(tau_n[2,2]))
qnorm(c(0.025,0.975),mean=mu_n[3],sd=sqrt(tau_n[3,3]))