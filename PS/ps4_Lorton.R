# ECON 5043: Homework 4 R script (3/12/2021)
# William Lorton
library(tidyverse)
library(geoR)
library(extraDistr)
library(MCMCpack)

########################################################################################################

# Q2 (CEX data):
cex.df <- read.csv("CEX.csv")

# 10900 weekly observations for 5450 households for 2018
# Focus on fruitveg variable which gives spending on fresh fruit/vegetables. 

## a.

# Getting mean, median, and standard deviation of fruitveg across entire sample.

cex.df %>%
  summarize(mean = mean(fruitveg),
            median = median(fruitveg),
            stdev = sd(fruitveg))

# Checking for difference in the median of fruitveg for the first week and second week.

  # Yes, there is a difference of 6.568995 (week 1) - 5.83379 (week 2).
cex.df %>%
  filter(week == 1) %>%
  summarize(median = median(fruitveg))

cex.df %>%
  filter(week == 2) %>%
  summarize(median = median(fruitveg))

# What fraction of the total sample spends $0 on fresh fruits/vegetables.
  
  # 3414/10900 observations spent $0 on these items.
cex.df %>%
  count(fruitveg == 0) %>%
  mutate(prop = n / sum(n))

## b.

# Giving histogram of fruitveg.

  # Indeed see significant bunching around $0.
  # --> "Censoring at zero"; optimal decisions were to spend zero on these items in a given week.
ggplot(data = cex.df, aes(x = fruitveg)) +
  geom_histogram(bins = 50)

hist(cex.df$fruitveg, breaks = 50)

## c.

# Transform the fruitveg data by applying a trigonometric function.

  # helps make data appear more normal; still spike at zero though.
cex.df$ihsfruitveg <- asinh(cex.df$fruitveg)
hist(cex.df$ihsfruitveg, breaks = 50)

## d.

# Perform Bayesian analysis on ihsfruitveg variable.
# Normal model with unknown mean and variance.

# Gather sample mean and sample variance.
ybar <- mean(cex.df$ihsfruitveg)
s_sq <- var(cex.df$ihsfruitveg)

# Define sample size, degrees of freedom, and total number of draws (want 1000).
samp <- length(cex.df$ihsfruitveg)
df2 <- samp - 1
total <- 1000

# Create placeholder vectors for the posterior variance, posterior mean, and posterior predictions.
post_norm_var<-rep(0, total)
post_norm_mu<-rep(0, total)

# Loop (two step process from lecture):
# Step 1: get a draw for the variance.
# Step 2: get a draw for the mean, conditional on what you just drew in (1) for the variance.
# Repeat the steps 1000 times. 

for (i in 1:total){
  
  # draw variance from scaled inverse chi-squared distribution with df = sample size - 1, scale = sample variance.
  post_norm_var[i] = geoR::rinvchisq(n=1, df2, scale=s_sq)
  #Sys.sleep(1)
  #print("Variance draw is")
  #print(post_norm_var[i])
  
  # draw mean from normal distribution with variance equal to the variance we just drew divided by sample size and
  # mean equal to sample mean.
  post_norm_mu[i] = rnorm(n=1, mean=ybar, sd=sqrt(post_norm_var[i]/samp))
  #Sys.sleep(1)
  #print("Mean draw is")
  #print(post_norm_mu[i])
  
}

# median of draws for mu|y:
median(post_norm_mu)
# seems reasonable
median(cex.df$ihsfruitveg)

# 95% credible intervals for the posterior mean and variance of ihsfruitveg:
quantile(post_norm_mu, c(0.025,0.975))
quantile(post_norm_var, c(0.025,0.975))

## e.

# Back-transform the draws for mu|y 
drawsback <- sinh(post_norm_mu)

# Histogram:
# Looks normal.
hist(drawsback, breaks = 50)


########################################################################################################

# Q3 (more work with CEX data):
# Predicting out-of-sample Data using the normal model with unknown mean and variance.

# It's possible to predict an out-of-sample data point using a generalized t-distribution.
# generalized t-dist has location parameter mu and scale parameter sigma.

## a.

# When mu = 0, sigma = 1, the generalized t-dist is equal to the t-dist.

# Get 500,000 draws of the generalized t-dist with df = 100, mu = 0, sigma = 1:
genstudentt <- rlst(n=500000, df=100, mu=0, sigma=1)

# Get 500,000 draws from the standard t-dist with df = 100:
studentt <- rt(n=500000, df=100)

# Showing that these are the same distributions:
hist(genstudentt, breaks = 50)
hist(studentt, breaks = 50)

## b.

# Forecast an out-of-sample data point, y tilde, in the normal model with unknown mean and
# unknown variance.
# The posterior predictive distribution is a generalized t-dist with df = (sample size - 1),
# mu = sample mean, sigma = sample standard deviation * sqrt(1 + 1/(sample size)).

# We use the ihsfruitveg variable to get 1,000 draws from this generalized t-dist.
samp <- length(cex.df$ihsfruitveg)
df <- samp - 1
mu <- mean(cex.df$ihsfruitveg)
sigma <- sd(cex.df$ihsfruitveg)*sqrt(1 + 1/samp)

predict_norm_mu <- rlst(n = 1000, df = df, mu = mu, sigma = sigma)

# Checking that median of prediction draws is similar draws for mu|y that we found before:
median(predict_norm_mu)
median(post_norm_mu)

########################################################################################################

# Q4 (multinomial likelihood with Dirichlet prior):

# Randomly survey 500 students about preferred mode of transportation to campus.
# Want to do inference on the probability that a student will choose one of seven transportation options.

# Data: 7 transportation categories
# y's represent counts; y1 first category, y2 second category, etc.
y1 = 51
y2 = 84
y3 = 101
y4 = 203
y5 = 32
y6 = 20
y7 = 9

# total sample size (should be 500):
n = (y1+y2+y3+y4+y5+y6+y7)
n

# Hyperparameters for Dirichlet prior.
# Set them to all be 0.01.
alpha1 = 0.01
alpha2 = 0.01
alpha3 = 0.01
alpha4 = 0.01
alpha5 = 0.01
alpha6 = 0.01
alpha7 = 0.01

# small prior sample size --> weakly informative prior.
sumalpha = (alpha1+alpha2+alpha3+alpha4+alpha5+alpha6+alpha7)
sumalpha

# Get 1,000 draws from the Dirichlet(a_i + y_i) posterior. 
# Save them in a 1000 x 7 posterior matrix.
total <- 1000

# posterior parameters:
post1<-alpha1+y1
post2<-alpha2+y2
post3<-alpha3+y3
post4<-alpha4+y4
post5<-alpha5+y5
post6<-alpha6+y6
post7<-alpha7+y7

# each row corresponds to a draw.
# each column corresponds to one of the 7 probabilities (i.e. posterior probability for one of
# the 7 transportation categories).
postmat <- rdirichlet(total, c(post1,post2,post3,post4,post5,post6,post7))
postmat

# Check that the posterior probabilities add to 1 for each draw --> each row sums to 1 since
# one of the transportation options must be selected for each person in the sample.
rowSums(postmat)

# Means of the posterior draws for the 7 probabilities; mean probability of choosing each option.
colMeans(postmat)

# Compare these to the relative frequency of the 7 options in the data.
c(y1/n, y2/n, y3/n, y4/n, y5/n, y6/n, y7/n)
# These clearly match up pretty well for all of the options.

