rm(list = ls())
library(MASS)
library(ggplot2)
library(matrixcalc)

setwd("C:/Users/admin/Downloads")

# Data are most recent available average electricity prices from EIA 
# Prices are average cents/kWh per customer type 
# In this example, k = 3 (residential, commercial, industrial)
data<-read.csv("ElectricityPrices.csv",header=T)

# We're using this as a good example of an MVN model use case because all three of the variables
# are highly correlated with one another; they would be good to model jointly!

# Mean of 3 data vectors (mean of res, mean of commercial, mean of industrial)
ybars = as.matrix(apply(data[,3:5],2,mean),nrow=3,ncol=1)
ybars

# Sample size (just use the row dimension of the data matrix)
n = dim(data)[1]
n

# Assume we know the true 3x3 covariance matrix (i.e. "known variance model")
# For simplicity, we will assume all variances = 25 and covariances = 20
# remember: main diagonal values are variances and off diagonal values are covariances
# byrow = TRUE --> input values three values per row at a time
sig = matrix(c(25,20,20,20,25,20,20,20,25),nrow=3,ncol=3,byrow=T)
sig

# Hyperparameters for a weakly-informative prior
# We know there should generally be a positive correlation between any two prices
# Let's set large variance terms (1e6) covariance terms (1e7)
# prior mean vector is zero for all elements.
# prior covariance matrix is full of very large values (1e6 for variances and 1e7 for covariances).
# result is a very diffuse, non-committal prior belief.
m = rbind(0,0,0)
# prior mean vector
m
gamma = matrix(c(1e6,1e7,1e7,1e7,1e6,1e7,1e7,1e7,1e6),nrow=3,ncol=3,byrow=T)
# prior covariance matrix
gamma

# Posterior mean and posterior variance (see lecture notes)
# solve() function gives inverse of a matrix
# matrix multiplication is done by %*%
mu_n = solve(solve(gamma) + n*solve(sig))%*%(solve(gamma)%*%m + n*solve(sig)%*%ybars)
# posterior mean vector
mu_n
tau_n = solve(solve(gamma)+n*solve(sig))
# posterior covariance matrix
tau_n

# Get 1,000 draws from the multivariate normal distribution (from MASS package)
?mvrnorm
# mu = posterior mean vector,
# Sigma = posterior covariance matrix
draws<-mvrnorm(n=1000,mu=mu_n,Sigma=tau_n)
# draws is a 1000 x 3 matrix
# each row is a draw that gives the average electricity price for residential, commercial, and industrial
# first column gives all draws for residential price
# second column gives all draws for commercial price
# third column gives all draws for industrial price
head(draws)

# Define data matrices for plotting histograms simultaneously
# vec() stacks columns of matrix to form a vector from the matrix; just for sake of plotting
# turns draws into a 3000 x 1 matrix
draws<-data.frame(vec(draws))
draws

# get name tag for each draw
drawscust<-as.matrix(c(rep("Res",1000),rep("Comm",1000),rep("Ind",1000)))
drawscust

# bind the name tags to the matrix with all the draws
# end up with 3000 x 2 matrix
drawsmat<-as.data.frame(cbind(draws,drawscust))
drawsmat
# add column names
colnames(drawsmat)<-c("Price","Customer")
head(drawsmat)

# Histograms of the posterior means 
plot<-ggplot(drawsmat, aes(Price, fill = Customer, color = Customer)) + geom_histogram(alpha = 0.40, aes(y = ..density..), position = 'identity', bins=40) + geom_density(alpha=0,size=1.1) + labs(title="Histogram of Posterior Means")
plot<-plot+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_blank(), axis.title.y = element_blank())
plot

# Could also just do plots for posterior means like so:
draws<-mvrnorm(n=1000,mu=mu_n,Sigma=tau_n)
# draws is a 1000 x 3 matrix
# each row is a draw that gives the average electricity price for residential, commercial, and industrial
# first column gives all draws for residential price
# second column gives all draws for commercial price
# third column gives all draws for industrial price
head(draws)
# residential draws histogram
hist(draws[,1], breaks = 50)
# commercial
hist(draws[,2], breaks = 50)
# industrial
hist(draws[,3], breaks = 50)

# The marginal posteriors are just one-variable normal distributions
# We can analyze the 95% credible intervals for the individual posterior means
# for mean of residential prices:
qnorm(c(0.025,0.975),mean=mu_n[1],sd=sqrt(tau_n[1,1]))
# for mean of commercial:
qnorm(c(0.025,0.975),mean=mu_n[2],sd=sqrt(tau_n[2,2]))
# for mean of industrial:
qnorm(c(0.025,0.975),mean=mu_n[3],sd=sqrt(tau_n[3,3]))
