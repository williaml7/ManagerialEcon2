# ECON 5043: Midterm R script
# 3/16/2021
# William Lorton

library(tidyverse)
library(MCMCpack)

# Q4:
# MVN model with known mean vector mu and unknown variance/covariance matrix Sigma.

# Posterior has inverse Wishart distribution with hyperparameters:
  # df = n + nu
  # scale matrix = S^0 + Lambda^-1

# define known means in known mean vector:
 # mu_1 = 14,
 # mu_2 = 11,
 # mu_3 = 8,

# Also create 3x3 matrix S^0 and define hyperparameters:
 # nu = 1,
 # Lambda^-1 = 3x3 Identity matrix

 # note that S^0 is the sum of squares matrix.

data<-read.csv("ElectricityPrices.csv",header=T)
mu = as.matrix(c(14,11,8),nrow=3,ncol=1)
n = dim(data)[1]
# Create 3x3 S0 matrix
Sfill = array(0,dim=c(3,3,51))
for (i in 1:n){
  st = as.matrix(rbind(data[i,3]-mu[1],data[i,4]-mu[2],data[i,5]-mu[3]),nrow=3,ncol=1)
  Sfill[,,i] = st%*%t(st)
}
S0row1 = cbind(sum(Sfill[1,1,]),sum(Sfill[1,2,]),sum(Sfill[1,3,]))
S0row2 = cbind(sum(Sfill[2,1,]),sum(Sfill[2,2,]),sum(Sfill[2,3,]))
S0row3 = cbind(sum(Sfill[3,1,]),sum(Sfill[3,2,]),sum(Sfill[3,3,]))
S0 = matrix(c(S0row1,S0row2,S0row3),nrow=3,ncol=3,byrow=T)
nu = 1
lambdainverse = matrix(1,nrow=3,ncol=3)

set.seed(5043)
# a.)

# Use riwish(v, S) function to get single draw from posterior for Sigma
# df:
v <- n + nu
v
# scale matrix:
S <- S0 + lambdainverse
S
?riwish

draw_invwish <- riwish(v = v, S = S)
draw_invwish

# compare first row/first column against variance of residential elec prices:
# the 1,1 element gives draw for variance of residential prices.
# 2,2 elements gives draw for variance of commercial prices.
# 3,3 element gives draw for variance of industrial prices.
var(data$Res)
draw_invwish
# Quite similar as expected; notice that the "prior sample size" (nu) is very small relative to
# our actual sample size of 51; thus, the prior belief concerning the var/covariance matrix has very
# little influence over our posterior inference; data will "speak relatively freely".

# b.)

# Set nu = 100 and get single draw from posterior:
nu <- 100
# df:
v <- n + nu
v
# scale matrix:
S <- S0 + lambdainverse
S

draw_invwish2 <- riwish(v = v, S = S)
draw_invwish2

# compare:
draw_invwish
draw_invwish2

# Drastically different. The second var/covariance matrix is filled with much smaller values than we should expect 
# according to the variance of the data. This second draw is clearly not coming from a well reasoned model. 
# The "prior sample size" nu is way too large here (note that we only have 51 actual data observations relative
# to the nu of 100).
# Main diagonal of variances doesn't match up with data variances whatsoever.
draw_invwish2
var(data$Res)
var(data$Com)
var(data$Ind)








