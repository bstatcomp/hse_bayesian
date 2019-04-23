library(rstan)
library(mcmcse)

# The data
dat <- data.frame(type = c("red", "yellow", "brown", "white-green", "green", "pink"),
                  count = c(3, 5, 7, 9, 9, 8))
dat
cat(sprintf("There are %d candy in total.\n", sum(dat$count)))

# 1) Proportion of brown candy & 3) Comparison with yellow candy
# If we can assume that the candy are generated independenty
# then the choice of model is fairly straightforward:
# we can use a Bernoulli model with theta representing
# the probability of brown candy:

if (!file.exists("../Lecture03 - Probabilistic programming/Temp/bernoulli.compiled")) {
  bernoulli_compiled <- stan_model("../Lecture03 - Probabilistic programming/bernoulli.stan")
  saveRDS(bernoulli_compiled, file = "../Lecture03 - Probabilistic programming/Temp/bernoulli.compiled")
} else {
  bernoulli_compiled <- readRDS("../Lecture03 - Probabilistic programming/Temp/bernoulli.compiled")  
}

# brown candy (estimate mean & 95% CI for mean)
n_brown <- dat$count[dat$type == "brown"]
dat_1 <- c(rep(1, n_brown), rep(0, sum(dat$count) - n_brown)) # 1 for brown, 0 for other
stan_data <- list(y = dat_1, n = length(dat_1))

samples <- sampling(bernoulli_compiled, data = stan_data,  # we supply the model and data
                    chains = 4, iter = 1200, warmup = 200)
theta_brown <- extract(samples)$theta
mcse(theta_brown)
quantile(theta_brown, p = c(0.025, 0.975))

# yellow candy (...)
n_yellow <- dat$count[dat$type == "yellow"]
dat_1 <- c(rep(1, n_yellow), rep(0, sum(dat$count) - n_yellow)) # 1 for brown, 0 for other
stan_data <- list(y = dat_1, n = length(dat_1))

samples <- sampling(bernoulli_compiled, data = stan_data,  # we supply the model and data
                    chains = 4, iter = 1200, warmup = 200)
theta_yellow <- extract(samples)$theta
mcse(theta_yellow)
quantile(theta_yellow, p = c(0.025, 0.975))

# 3) Comparison
mcse(theta_brown > theta_yellow)




# 2) Number of pieces of candy

# Understanding the Poisson rate
# - by varying lambda, we can get a feel for where
# lambda could be.
library(ggplot2)
res <- NULL
for (i in c(1, 5, 10, 20, 50, 100)) {
  res <- rbind(res, 
               data.frame(
                 Sample = rpois(1000, i),
                 Lambda = i))
}
g1 <- ggplot(res, aes(x = Sample)) + geom_histogram(binwidth = 1) +
  facet_wrap(. ~ Lambda, ncol = 2)
plot(g1)

# based on this I would, before even seeing the data,
# assume that the Poisson rate parameter will be somewhere
# between 5 and 100. Note that we could probably use a more informative
# prior here if we put some more effort into thinking about where the 
# true probability might be, before looking at the data.

# The model
if (!file.exists("poisson.compiled")) {
  poisson_compiled <- stan_model("poisson.stan")
  saveRDS(poisson_compiled, file = "poisson.compiled")
} else {
  poisson_compiled <- readRDS("poisson.compiled")  
}

y <- c(41, 47, 33, 32, 38, 44) # data from other groups
stan_data <- list(y = c(y),
                  n = length(y))

samples <- sampling(poisson_compiled, data = stan_data,  # we supply the model and data
                    chains = 1, iter = 1200, warmup = 200)

lambda <- extract(samples)$lambda
# mean and confidence interval for lambda
mcse(lambda)
quantile(lambda, p = c(0.025, 0.975))

# confidence interval on count for new/unseen bag
y_new <- c()
for (l in lambda) {
  y_new <- c(y_new, rpois(1, l))
}
quantile(y_new, c(0.025, 0.975))


# 4/5) Joint model for count and all six candy types
# The model is described in the comments of the .stan file:
writeLines(readLines("candy.stan"))

if (!file.exists("candy.compiled")) {
  candy_compiled <- stan_model("candy.stan")
  saveRDS(candy_compiled, file = "candy.compiled")
} else {
  candy_compiled <- readRDS("candy.compiled")  
}

# the data: I had just one bag
y <- c()
for (i in 1:6) {
  y <- c(y, rep(i, dat$count[i]))
}
stan_data <- list(n = length(y), m = 1, k = 6,
                  y = y, b = rep(1, length(y)))

# sample from the posterior
samples <- sampling(candy_compiled, data = stan_data,  # we supply the model and data
                    chains = 1, iter = 1200, warmup = 200)

probs  <- extract(samples)$candy_probs
lambda <- extract(samples)$lambda

quantile(lambda, p = c(0.025, 0.975))   # CI for lambda
mean(probs[,3] > probs[,2])             # proportion of brown > proportion of yellow?

# is any of the candy types clearly more frequent? (same as finding the "best" pig from L04_Piglets_ANOVA.r)
winner <- array(0, dim = 6)

for (j in 1:nrow(probs)) {
  x <- probs[j,]
  winner[order(-x)[1]] <- winner[order(-x)[1]] + 1 
}

winner / nrow(probs) # we can't claim with high certainty that one type is clearly more frequent

 
