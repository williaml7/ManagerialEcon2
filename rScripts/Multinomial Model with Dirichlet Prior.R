rm(list = ls())

# load the MCMC package
library(MCMCpack)

# Data: 8 options for energy choice
# counts for each option
y1 = 23
y2 = 27
y3 = 9
y4 = 6
y5 = 12
y6 = 13
y7 = 2
y8 = 8

# sample size
n=(y1+y2+y3+y4+y5+y6+y7+y8)
n

# Hyperparameters
# Set them to all be small numbers since we want the "prior sample size" to be small
alpha1 = 1
alpha2 = 1
alpha3 = 1
alpha4 = 1
alpha5 = 1
alpha6 = 1
alpha7 = 1
alpha8 = 1

sumalpha=(alpha1+alpha2+alpha3+alpha4+alpha5+alpha6+alpha7+alpha8)
sumalpha

# Get 10,000 draws from the posterior for each of the probabilities 
# Save them in a 10000 x 8 matrix posterior matrix
total<-10000

# posterior parameters
post1<-alpha1+y1
post2<-alpha2+y2
post3<-alpha3+y3
post4<-alpha4+y4
post5<-alpha5+y5
post6<-alpha6+y6
post7<-alpha7+y7
post8<-alpha8+y8

# store in 10,000 row, 8 column matrix
# each row corresponds to a draw
# each column corresponds to one of the 8 probabilities
postmat<-rdirichlet(total,c(post1,post2,post3,post4,post5,post6,post7,post8))

# First, it's nice to check that, for each draw, the posterior probabilities add to 1
rowSums(postmat)

# Histograms (just look at the first probability, for example; probability of coal)
hist(postmat[,1],breaks=50)

# Notice that this histogram looks like a beta distribution
# It is a beta distribution. As it turns out, it's beta (alpha1+y1, sumalpha+n-(alpha1+y1))
hist(rbeta(10000,shape1=alpha1+y1,shape2=sumalpha+n-alpha1-y1),breaks=50)

# Means of the posterior draws for the eight probabilities; option 1, option 2, ..., option 8 probability
colMeans(postmat)

# Compare these to the relative frequency of the eight options in the data; ideally, will be similar.
c(y1/n,y2/n,y3/n,y4/n,y5/n,y6/n,y7/n,y8/n)

############################################################################
# What would happen if we used a very informative prior?
alpha1 = 100
alpha2 = 100
alpha3 = 100
alpha4 = 100
alpha5 = 100
alpha6 = 100
alpha7 = 100
alpha8 = 100

sumalpha = (alpha1+alpha2+alpha3+alpha4+alpha5+alpha6+alpha7+alpha8)
# very large prior sample size (8 times larger than actual sample size)
sumalpha

# set posterior parameters
post1<-alpha1+y1
post2<-alpha2+y2
post3<-alpha3+y3
post4<-alpha4+y4
post5<-alpha5+y5
post6<-alpha6+y6
post7<-alpha7+y7
post8<-alpha8+y8

# Re-do the posterior draws
postmat2<-rdirichlet(total,c(post1,post2,post3,post4,post5,post6,post7,post8))

# How do the means of the posterior draws compare to the relative frequencies?
# Poorly
# prior is exerting very strong influence on posterior inference!
# all probabilities are very similar in the posterior despite the data not showing this at all.
colMeans(postmat2)
c(y1/n,y2/n,y3/n,y4/n,y5/n,y6/n,y7/n,y8/n)
