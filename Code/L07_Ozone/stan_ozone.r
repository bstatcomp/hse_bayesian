library(ggplot2)
library(rstan)
library(randomForest)
library(glmnet)

# Load and prepare data --------------------------------------------------------
dataset <- readRDS("../datasets/ozone.rds")
dataset <- dataset[,2:402]

set.seed(0)
idx          <- sample(1:nrow(dataset), 500, rep = F)
dataset[,-1] <- scale(dataset[,-1]) # in practice, test data should not be used 
                                      # to scale all data, but we do it to simplify
dat_train    <- dataset[ idx,]
dat_test     <- dataset[-idx,]


# Compare different models -----------------------------------------------------
res_tr <- NULL
res_te <- NULL
options(stringsAsFactors = FALSE)

# linear regression
my_lm <- lm(target ~ ., dat_train)

res_tr <- rbind(res_tr, data.frame(Method = "lm", 
                             Row = 1:nrow(dat_train),
                             Predicted = predict(my_lm),
                             True = dat_train$target))
res_te <- rbind(res_te, data.frame(Method = "lm", 
                                Row = 1:nrow(dat_test),
                                Predicted = predict(my_lm, newdata = dat_test),
                                True = dat_test$target))

# random forests
my_rf <- randomForest(target ~ ., dat_train, ntree = 1000)

res_tr <- rbind(res_tr, data.frame(Method = "rf", 
                                   Row = 1:nrow(dat_train),
                                   Predicted = predict(my_rf),
                                   True = dat_train$target))
res_te <- rbind(res_te, data.frame(Method = "rf", 
                                   Row = 1:nrow(dat_test),
                                   Predicted = predict(my_rf, newdata = dat_test),
                                   True = dat_test$target))

# L2-regularized regression, cross-validated reg. parameter
cvfit <- glmnet::cv.glmnet(x = as.matrix(dat_train[,-1]), y = dat_train[,1],
                           alpha = 0)

res_tr <- rbind(res_tr, data.frame(Method = "L2", 
                                   Row = 1:nrow(dat_train),
                                   Predicted = c(predict(cvfit, newx = as.matrix(dat_train[,-1]), s = "lambda.1se")),
                                   True = dat_train$target))
res_te <- rbind(res_te, data.frame(Method = "L2", 
                                   Row = 1:nrow(dat_test),
                                   Predicted = c(predict(cvfit, newx = as.matrix(dat_test[,-1]), s = "lambda.1se")),
                                   True = dat_test$target))
cvfit_L2 <- cvfit


# L1-regularized regression, cross-validated reg. parameter
cvfit <- glmnet::cv.glmnet(x = as.matrix(dat_train[,-1]), y = dat_train[,1],
                           alpha = 1)

res_tr <- rbind(res_tr, data.frame(Method = "L1", 
                                   Row = 1:nrow(dat_train),
                                   Predicted = c(predict(cvfit, newx = as.matrix(dat_train[,-1]), s = "lambda.1se")),
                                   True = dat_train$target))
res_te <- rbind(res_te, data.frame(Method = "L1", 
                                   Row = 1:nrow(dat_test),
                                   Predicted = c(predict(cvfit, newx = as.matrix(dat_test[,-1]), s = "lambda.1se")),
                                   True = dat_test$target))
cvfit_L1 <- cvfit


# L2 regularized Bayesian (with hyperprior on reg. parameter)
model_fit <- stan_model(file = "lin_reg_model.stan")

y <- dat_train[, 1]
X <- dat_train[,-1]
X_test <- dat_test[,-1]
stan_data <- list(y = y, X = X, n = nrow(X), k = ncol(X), m = nrow(X_test),
                  X_test = X_test)

smp <- sampling(model_fit,  data = stan_data, 
              iter = 1500, warmup = 500, chains = 1, verbose = T, 
              seed = 1)

res_tr <- rbind(res_tr, data.frame(Method = "L2bayes", 
                                   Row = 1:nrow(dat_train),
                                   Predicted = colMeans(smp$pred),
                                   True = dat_train$target))
res_te <- rbind(res_te, data.frame(Method = "L2bayes", 
                                   Row = 1:nrow(dat_test),
                                   Predicted = colMeans(smp$pred_test),
                                   True = dat_test$target))


# compare predictive quality
se_lb <- function(x) {mean(x) - sd(x) / sqrt(length(x))}
se_ub <- function(x) {mean(x) + sd(x) / sqrt(length(x))}
tmp <- rbind(data.frame(res_te, Data = "test"),
             data.frame(res_tr, Data = "train"))
g1 <- ggplot(tmp, aes(x = Method, y = (Predicted - True)^2, colour = Method)) + 
  facet_wrap(~Data) +
  stat_summary(fun.data = "mean_cl_boot")
ggsave(g1, file = "ozone_MSE.pdf", width = 10, height = 5)

# compare coefficients
coeff_L1  <- as.matrix(glmnet::coef.glmnet(cvfit_L1, s = "lambda.1se"))[-1]
coeff_L2  <- as.matrix(glmnet::coef.glmnet(cvfit_L2, s = "lambda.1se"))[-1]
coeff_L2b <- colMeans(extract(smp)$beta)
coeff_lm  <- my_lm$coefficients[-1]
coeff_lm[is.na(coeff_lm)] <- 0

tmp <- data.frame(Idx = rep(1:400, 4), 
                  Coefficient = c((abs(coeff_L1)), (abs(coeff_L2)), (abs(coeff_L2b)), (abs(c(coeff_lm)))),
                  Method = rep(c("L1", "L2", "L2bayes","lm"), each = 400))

g1 <- ggplot(tmp, aes(x = Idx, y = (Coefficient), group = Method, fill = Method)) +
  geom_bar(stat = "identity") + facet_wrap(~Method, ncol = 2, scales = "free_y") + 
  ylab("|coefficient|") + ylab("input variable index")
ggsave(g1, file = "ozone_coeff.pdf", width = 7, height = 6)
