# Sample size
n = 100

# Parameters of the beta distribution
alpha = 0.25
beta = 0

# Simulate some data
set.seed(1)
x = rbeta(n, alpha, beta)

# Note that the distribution is not symmetrical
curve(dbeta(x,alpha,beta))

#example CI
library(simpleboot)
x.boot = one.boot(x, mean, R=10^4)
hist(x.boot)                # Looks good
boot.ci(x.boot, type="bca")
