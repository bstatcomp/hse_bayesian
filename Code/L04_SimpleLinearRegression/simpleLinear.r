# Simple linear regression example ---------------------------------------------
library(ggplot2)
library(mcmcse)
library(reshape2)

# read dataset
dataset <- read.table("../datasets/simpleReg.csv", sep = ",", h = T)

# plot the data
g1 <- ggplot(dataset, aes(x = X, y = Y)) + geom_point()
ggsave("SimpleReg01.pdf", g1, width = 4, height = 4)

# plot several proposed lines
g1 <- ggplot(dataset, aes(x = X, y = Y)) + geom_point() +
  geom_abline(intercept = 37, slope = -5, colour = "red") +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  geom_abline(intercept = 0, slope = -2.5, colour = "green") +
  geom_abline(intercept = 0, slope = 10, colour = "orange")
ggsave("SimpleReg02.pdf", g1, width = 4, height = 4)

# plot the linear regression line and diagnose residuals
g1 <- ggplot(dataset, aes(x = X, y = Y)) + geom_point() + 
  geom_smooth(colour = "blue", method = "lm", formula = y ~ x + 1, se = F)
ggsave("SimpleReg03.pdf", g1, width = 4, height = 4)

# Bayesian linear regression via Gibbs sampling --------------------------------

bayes_lm <- function(y, X, m = 5000, 
                     beta0 = array(0, dim = ncol(X)),
                     Sigma0 = diag(ncol(X)) * 2500,
                     a0 = 1, b0 = 20,
                     s2_0 = 5) {
  
  library(MASS)
  
  smp_s2   <- s2_0
  all_s2   <- NULL
  all_beta <- NULL
  
  # Gibbs sampling for the posterior
  for (i in 1:m)
  {
    # updating beta  
    tmp.cov  <- solve(solve(Sigma0) + t(X) %*% X / smp_s2)
    tmp.mu   <- tmp.cov %*% (solve(Sigma0) %*% beta0 + t(X) %*% y / smp_s2)
    smp_beta <- mvrnorm(1, tmp.mu, tmp.cov)
    
    # updating s2
    SSR <- t(y) %*% y - 2 * t(smp_beta) %*% t(X) %*% y +  t(smp_beta) %*% t(X) %*% X %*% smp_beta
    smp_s2 <- 1 / rgamma(1, a0 + length(y)/2, b0 + 0.5 * SSR)
  
    # add new samples
    all_s2 = rbind(all_s2, sqrt(smp_s2)) # we'll monitor the standard deviation
    all_beta = rbind(all_beta, smp_beta)
  }
  
  data.frame(s2 = all_s2, beta = all_beta)
}


set.seed(0)
y   <- dataset$Y
X   <- cbind(dataset$X, 1) # intercept (constant input variable)
tmp <- bayes_lm(y, X)

# MCMC diagnostics
print(ess(tmp))
print(apply(tmp, 2, mcse))

# trace plots
g1 <- ggplot(melt(data.frame(Sample = 1:nrow(tmp), tmp), id.vars = "Sample"), 
             aes(x = Sample, y = value, group = variable)) + geom_line() + 
  xlab("sample") + ylab("value") + facet_wrap(~variable, ncol = 1, scales = "free") +
  theme(axis.text.x = element_blank())
ggsave("SimpleReg04.pdf", g1, width = 6, height = 6)


# plot the regression line
g1 <- ggplot(dataset, aes(x = X, y = Y)) + geom_point() 
for (i in sample(1:nrow(tmp), 500, rep = T)) {
  g1 <- g1 + geom_abline(alpha = 0.01, colour = "red", slope = tmp$beta.1[i], intercept = tmp$beta.2[i]) 
}
plot(g1)
ggsave("SimpleReg05.pdf", g1, width = 4, height = 4)