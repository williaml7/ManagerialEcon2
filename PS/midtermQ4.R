rm(list = ls())
library(MCMCpack)

setwd("C:/Users/Admin/Downloads")
data<-read.csv("ElectricityPrices.csv",header=T)

# Suppose true means are:
mu = as.matrix(c(14,11,8),nrow=3,ncol=1)

# Sample size (just use the row dimension of the data matrix)
n = dim(data)[1]

# Create 3x3 S0 matrix 
# Create placeholder "sfill" that will still each of the elements of S0
Sfill = array(0,dim=c(3,3,51))

# Loop that fills in each element of S0 matrix
for (i in 1:n){
  st = as.matrix(rbind(data[i,3]-mu[1],data[i,4]-mu[2],data[i,5]-mu[3]),nrow=3,ncol=1)
  Sfill[,,i] = st%*%t(st)
}

# S0 is just 
S0row1 = cbind(sum(Sfill[1,1,]),sum(Sfill[1,2,]),sum(Sfill[1,3,]))
S0row2 = cbind(sum(Sfill[2,1,]),sum(Sfill[2,2,]),sum(Sfill[2,3,]))
S0row3 = cbind(sum(Sfill[3,1,]),sum(Sfill[3,2,]),sum(Sfill[3,3,]))

S0 = matrix(c(S0row1,S0row2,S0row3),nrow=3,ncol=3,byrow=T)

# Hyperparameters
nu = 1
lambdainverse = matrix(1,nrow=3,ncol=3)

set.seed(5043)
