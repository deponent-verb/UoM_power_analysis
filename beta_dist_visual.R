# Sample size
n = 100

# Parameters of the beta distribution
alpha = 5
beta = 0.01

# Simulate some data
set.seed(1)
x = rbeta(n, alpha, beta)

# Note that the distribution is not symmetrical
curve(dbeta(x,alpha,beta))

