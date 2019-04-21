library(ggplot2)
library(psych) # for pairs.panels
library(rstanarm)
library(mcmcse)
library(reshape2)
library(rstan)

dataset <- read.table("../code/datasets/bears.csv", sep = "\t", h = T)
dataset <- dataset[,-2] # we won't use the month when measurements were taken

#pairs.panels(dataset[,1:7])

# linear model -----------------------------------------------------------------
tmp    <- stan_lm(WEIGHT ~ ., data = dataset, prior = NULL, chains = 1)
output <- tmp$stanfit

post_pred <- rstanarm::posterior_predict(tmp)

# Visualize --------------------------------------------------------------------

# predicted vs observed
p_mu <- colMeans(post_pred)
p_ub <- apply(post_pred, 2, quantile, p = 0.975)
p_lb <- apply(post_pred, 2, quantile, p = 0.025)
dat  <- data.frame(mu = p_mu, ub = p_ub, lb = p_lb, true = dataset$WEIGHT)

g1 <- ggplot(dat, aes(x = p_mu, y = true, xmin = lb, xmax = ub)) + 
  xlab("predicted") + ylab("observed") +
  xlim(-150,500) + ylim(-150,500) + geom_point() + 
  geom_errorbarh() +
  geom_abline(aes(slope = 1, intercept = 0), lty = "dashed", colour = "red")
plot(g1)

# predicted vs residual
dat  <- data.frame(mu = dat$mu, resid = dat$true - dat$mu)

g1 <- ggplot(dat,aes(x = mu, y = resid)) + xlab("predicted") + ylab("residual") +
  geom_point() + xlim(0,500) +
  geom_abline(aes(slope = 0, intercept = 0), lty = "dashed", colour = "red")
plot(g1)

plot(plot(output, par = names(dataset)[-8], include = T))
