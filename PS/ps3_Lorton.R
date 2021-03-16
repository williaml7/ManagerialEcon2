# ECON 5043: Homework 3 R script
# William Lorton
library(tidyverse)
library(geoR)

# Q4
# get data on avg duration of unemployment (in weeks) taken on a monthly basis 1948-2020:
df <- read.csv("Duration.csv")

z# Q4 a.)
# mean, median, and standard deviation of the weeks variable in 50s, 60s, 70s, 80s, 90s, 2000s, 2010s:
# 1950s:
df %>% 
  select(Year, Weeks) %>%
  filter(Year >= 1950 & Year <= 1959) %>%
  summarize(mean.1950s = mean(Weeks),
            median.1950s = median(Weeks),
            sd.1950s = sd(Weeks))
# 1960s:
df %>% 
  select(Year, Weeks) %>%
  filter(Year >= 1960 & Year <= 1969) %>%
  summarize(mean.1960s = mean(Weeks),
            median.1960s = median(Weeks),
            sd.1960s = sd(Weeks))
# 1970s:
df %>% 
  select(Year, Weeks) %>%
  filter(Year >= 1970 & Year <= 1979) %>%
  summarize(mean.1970s = mean(Weeks),
            median.1970s = median(Weeks),
            sd.1970s = sd(Weeks))
# 1980s:
df %>% 
  select(Year, Weeks) %>%
  filter(Year >= 1980 & Year <= 1989) %>%
  summarize(mean.1980s = mean(Weeks),
            median.1980s = median(Weeks),
            sd.1980s = sd(Weeks))
# 1990s:
df %>% 
  select(Year, Weeks) %>%
  filter(Year >= 1990 & Year <= 1999) %>%
  summarize(mean.1990s = mean(Weeks),
            median.1990s = median(Weeks),
            sd.1990s = sd(Weeks))
# 2000s:
df %>% 
  select(Year, Weeks) %>%
  filter(Year >= 2000 & Year <= 2009) %>%
  summarize(mean.2000s = mean(Weeks),
            median.2000s = median(Weeks),
            sd.2000s = sd(Weeks))
# 2010s:
df %>% 
  select(Year, Weeks) %>%
  filter(Year >= 2010 & Year <= 2019) %>%
  summarize(mean.2010s = mean(Weeks),
            median.2010s = median(Weeks),
            sd.2010s = sd(Weeks))

# overall mean, median and standard deviation of weeks variable:
mean(df$Weeks)
median(df$Weeks)
sd(df$Weeks)

# Looking for outliers in 2020:
df %>%
  select(Year, Weeks) %>%
  filter(Year == 2020)
# April, May, and perhaps June and July are outliers; lots of newly unemployed due to covid. 

# Q4 c.)

# Have exponential likelihood
# conjugate gamma prior: theta ~ Gamma(alpha, beta)
# and thus a gamma posterior: theta|y ~ Gamma(alpha + sample.size, beta + sample.size*sample.mean) 

# We set alpha = beta = 0.001 for prior mean E(theta) = 1 and prior variance Var(theta) = 1000
alpha <- 0.001
beta <- 0.001

sample.size <- length(df$Weeks)
sample.mean <- mean(df$Weeks)

# Get 10k draws from our gamma posterior for this model:
# We invert the scale parameter since R inverts it from the definition of the gamma pdf in our book.
post_exp <- rgamma(10000, 
                   shape = alpha + sample.size, 
                   scale = 1/(sample.size*sample.mean + beta))

# Looking at the draws:
hist(post_exp, breaks=50)
mean(post_exp)
median(post_exp)
# The 95% credible interval
qgamma(c(0.025,0.975), 
       shape = alpha + sample.size, 
       scale = 1/(sample.size*sample.mean + beta))
# Or can get 95% credible interval from draws:
quantile(post_exp, c(0.025, 0.975))

# R code that Dr. McFadden gave me to check reasonableness of posterior draws:
check <- rexp(n = 1000, rate = mean(post_exp))
mean(check)
mean(df$Weeks)
hist(check, breaks = 50)
hist(df$Weeks, breaks = 50)
# Seems fine.

# Q4 d.)

# Drop last 12 observations from the sample, assuming 2020 is an outlier
df2 <- df %>% filter(Year < 2020)

# Repeat part c.)
alpha <- 0.001
beta <- 0.001

sample.size <- length(df2$Weeks)
sample.mean <- mean(df2$Weeks)

post_exp2 <- rgamma(10000, 
                   shape = (alpha + sample.size), 
                   scale = 1/(sample.size*sample.mean + beta))

hist(post_exp2, breaks=50)
mean(post_exp2)
median(post_exp2)
quantile(post_exp2, c(0.025, 0.975))

# Can check reasonableness again:
check <- rexp(n = 1000, rate = mean(post_exp2))
mean(check)
mean(df2$Weeks)
hist(check, breaks = 50)
hist(df2$Weeks, breaks = 50)

# Results don't change very much after dropping 2020:
mean(post_exp)
mean(post_exp2)
median(post_exp)
median(post_exp2)

# Q5
# Have 10 observations on hourly wages.
# Assume data are iid normal with known mean = $20/hr.
y <- c(18,17,21,23,20,19,23,16,20,20)

# Normal model likelihood with known mean (20) and unknown variance.
# Prior is scaled inverse chi squared(nu0, sig02)
# Posterior is scaled inverse chi squared (df2, scale2):
#   df2     = nu0 + sample.size
#   scale2  = (nu0*sig02+sample.size*v)/(nu0+sample.size)
#   v       = (1/sample.size)*sum((y-theta)^2)

sample.size <- length(y)
theta <- 20

# Q5 a.)

v <- (1/sample.size)*sum((y-theta)^2)
# v = 4.9

# Q5 c.)

# Assume nu0 = 1, sig02 = 1 
# --> nu0 = 1 prior observation ("prior sample size of 1")
# and average squared deviation of sigma02 = 1.
nu0 <- 1
sig02 <- 1

# Getting 1k draws from the posterior:

df2 <- nu0 + sample.size
scale2 <- (nu0*sig02 + sample.size*v)/(nu0 + sample.size)

post_normal_var <- rinvchisq(1000,
                             df = df2,
                             scale = scale2)

# Histogram and median of draws:

hist(post_normal_var, breaks = 50)
median(post_normal_var)

var(y)
# Median of draws is somewhat close to the sample variance:
abs(var(y) - median(post_normal_var))

# Q5 d.)

# Now we assume nu0 = 10, sig02 = 1
# --> prior sample size of 10 which matches actual data sample size
# and low variance in prior belief still.
nu0 <- 10
sig02 <- 1

# Getting 1k draws from the posterior:

df2 <- nu0 + sample.size
scale2 <- (nu0*sig02 + sample.size*v)/(nu0 + sample.size)

post_normal_var <- rinvchisq(1000,
                             df = df2,
                             scale = scale2)

# Histogram and median of draws:

hist(post_normal_var, breaks = 50)
median(post_normal_var)

var(y)
# Median variance of draws is somewhat close to the sample variance:
abs(var(y) - median(post_normal_var))

# There is a relatively large difference between the data variance and the variances from the posterior
# draws now; the prior belief is exhibiting strong influence on our posterior inference
# since the "prior sample size" is relatively large and the prior average squared deviation is 
# quite low.







