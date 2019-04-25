dat <- readRDS(file = "data_train.rds")
dat_test <- readRDS(file = "data_test.rds")

dat_test <- dat_test[,-which(names(dat_test) == "Yield")]

library(rstanarm)
library(rstan)



# linear model -----------------------------------------------------------------
tmp    <- stan_lm(Yield ~ ., data = dat, prior = NULL, chains = 1, iter = 300)
output <- tmp$stanfit
post_pred <- rstanarm::posterior_predict(tmp, newdata = dat_test)

saveRDS(t(post_pred), "eriks_predictions.rds")

predictions <- t(post_pred) 

final <- NULL

for (i in 1:nrow(predictions)) {
  x <- predictions[i,]
  tmp <- table(cut(x, breaks = c(seq(0, 150, 10), Inf))) 
  tmp <- tmp/sum(tmp) 
  final <- rbind(final, tmp)
}

write.table(final, file = "predictions.csv", col.names = T,
            row.names = F, sep = ",")