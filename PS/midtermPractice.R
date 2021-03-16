# Midterm practice (3/15/2020)

# Q4

# Assume the number of countries to which you export in any year is independent with
# a Poisson(theta) distribution.
# Assume a conjugate Gamma(alpha, beta) prior for theta.

# a.)
# --> Poisson Model:
# Prior is gamma(alpha, beta)
# Posterior is gamma(alpha + samp*ybar, beta + samp)

# b.)
# data:
df <- data.frame(c(25,31,31,22,21,26,20,16,22,24), c(seq(from = 2011, to = 2020, by = 1)))
colnames(df) <- c("numCountries.export", "Year")

samp <- length(df$numCountries.export)
ybar <- mean(df$numCountries.export)


# c.)
alpha <- 0.0001
beta <- 0.0001

# posterior mean:
# Gamma mean = alpha / beta
(alpha + samp*ybar)/(beta + samp)

# posterior variance:
# Gamma variance = alpha / beta^2
(alpha + samp*ybar)/(beta + samp)^2

# d.)
# Want to predict the number of trading countries in 2021.
# We are given that the posterior predictive distribution is a negative
# binomial distribution: y-tilde | y ~ NegBin(alpha + samp*ybar, 1/(beta + samp + 1))

# Want to get 1,000 draws from this distribution.
?rnbinom
# There's some discrepancy between the pdf we want to use and the one R uses:
# the second parameter is adjusted to prob = p = (beta + samp)/(beta + samp + 1)

# e.)

# Getting 1,000 draws from this posterior predictive distribution.
# must use:
predict_draws_poisson <- rnbinom(n = 1000, size = alpha + samp*ybar, prob = (beta + samp)/(beta + samp + 1))

# median of the draws:
median(predict_draws_poisson)
median(df$numCountries.export)

# 95% credible interval:
quantile(predict_draws_poisson, c(0.025, 0.975))
qnbinom(c(0.025, 0.975), size = alpha + samp*ybar, prob = (beta + samp)/(beta + samp + 1))
