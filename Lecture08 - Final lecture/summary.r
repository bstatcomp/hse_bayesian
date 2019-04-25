dat <- readRDS(file = "data_train.rds")



library(rstanarm)
library(rstan)



# linear model -----------------------------------------------------------------
tmp    <- stan_lm(Yield ~ ., data = dat, prior = NULL, chains = 1, iter = 300)
output <- tmp$stanfit

post_pred <- rstanarm::posterior_predict()
