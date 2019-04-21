### FROG

# transition matrix
q    <- matrix(rep(0, 9), nrow = 3)
q[1,] <- c(0.0, 0.5, 0.5)
q[2,] <- c(0.5, 0.0, 0.5)
q[3,] <- c(0.5, 0.5, 0.0)

m  <- 10000
x  <- 1

set.seed(12345)
for (i in 1:m) {
  x <- c(x, sample(1:3, 1, prob = q[x[i],])) # sample next state
}

table(x) / (m + 1)

### FLY
m  <- 10000
x  <- 0.5

set.seed(1010101)
for (i in 1:m) {
  # propose next state
  x_new <- x[i] + runif(1, -0.5, +0.5)
  # wrap-around
  if (x_new < 0) x_new <- 0 - x_new
  if (x_new > 1) x_new<- 2 - x_new
  # add new state
  x <- c(x, x_new)
}

hist(x, breaks = 20)


### 0-1
q    <- matrix(rep(0, 4), nrow = 2)
q[1,] <- c(0.5, 0.5)
q[2,] <- c(0.5, 0.5)

m  <- 10000
x  <- 1

set.seed(12345)
for (i in 1:m) {
  x <- c(x, sample(1:2, 1, prob = q[x[i],])) # sample next state
}

x <- x - 1 # get 0-1 instead of 1-2
table(x) / (m + 1)



### Bayesian posterior

# We're working with a simple Bernoulli-Beta model
# Prior Beta(1,1), 12 data points, 9 ones, 3 zeroes
# The posterior is then Beta(10, 4)
# We're going to calculate posterior probability of parameter
# being greater than 0.75
library(mcmcse)
library(ggplot2)
a0 <- 1; b0 <- 1
y <- c(rep(1, 9), rep(0, 3))
n <- length(y)

## 1. Computation based on deriving that the posterior is Beta(10, 4)
x  <- seq(0, 1, 0.01)
xx <- data.frame(x = x, y = dbeta(x, a0 + sum(y), b0 + n - sum(y)), type = "posterior") 
ggplot(xx, aes(x = x, y = y, group = type, colour = type)) + geom_line() + xlab("theta") + ylab("density")
1 - pbeta(0.75, a0 + sum(y), b0 + n - sum(y))

## 2. Computation based on just integrating the non-normalized
## posterior and normalizing
prop_posterior <- function(theta, a0, b0, y) {
  dbinom(sum(y), length(y), theta) * dbeta(theta, a0, b0)
}
p1 <- integrate(prop_posterior, lower = 0.75, upper = 1.00, a0, b0, y)$value
p2 <- integrate(prop_posterior, lower = 0.00, upper = 0.75, a0, b0, y)$value
p1 / (p1 + p2) # normalization

## 3. Computation based on Monte Carlo approximation 
## with actual distribution
set.seed(0)
m <- 1000
x <- rbeta(m, a0 + sum(y), b0 + n - sum(y))
mcse(x > 0.75)

## 4. Computation based on Monte Carlo approximation 
## with prop_posterior and Metropolis algorithm (MCMC)
set.seed(0)
m <- 10000
x <- 0.5 # starting position
for (i in 1:m) {
  # proposing a new state (with wrap-around)
  x_new <- x[i] + runif(1, -0.5, +0.5)
  if (x_new < 0) x_new <- 0 - x_new
  if (x_new > 1) x_new<- 2 - x_new
  
  # Metropolis correction
  p_curr <- prop_posterior(x[i], a0, b0, y)
  p_new  <- prop_posterior(x_new, a0, b0, y)
  acceptance_prob <- min(p_new / p_curr, 1.0)
  if (runif(1) > acceptance_prob) {
    # reject
    x_new <- x[i]
  }
  
  # add state
  x <- c(x, x_new)
}
hist(x)
mcse(x > 0.75)

## 5. Computation using Stan
library(rstan)
if (!file.exists("../Lecture03 - Probabilistic programming/Temp/bernoulli.compiled")) {
  bernoulli_compiled <- stan_model("../Lecture03 - Probabilistic programming/bernoulli.stan")
  saveRDS(bernoulli_compiled, file = "../Lecture03 - Probabilistic programming/Temp/bernoulli.compiled")
} else {
  bernoulli_compiled <- readRDS("../Lecture03 - Probabilistic programming/Temp/bernoulli.compiled")  
}

stan_data <- list(y = y,
                  n = length(y))

samples <- sampling(bernoulli_compiled, data = stan_data,  # we supply the model and data
                    chains = 1, iter = 1200, warmup = 200)
x <- extract(samples)$theta
hist(x)
mcse(x > 0.75)

