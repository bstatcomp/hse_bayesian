# Select best model according to MSE with Cross-Validation ---------------------

library(randomForest)
library(ggplot2)
set.seed(0)
degr <- c(1,2,3,5,25)

# load dataset and add higher order terms
dataset <- read.table("../datasets/simpleReg.csv", sep = ",", h = T)
x <- as.matrix(dataset$X)
for (i in 2:max(degr)) x = cbind(x, x[,1]^i)
y <- as.matrix(dataset$Y)

# in-sample mean squared error (MSE) -------------------------------------------
res.is = NULL
for (i in 1:length(degr))
{
  lr <- lm(y ~ x[,1:degr[i]])
  py <- predict(lr)
  res.is = rbind(res.is, data.frame(Model = degr[i], MSE = mean((y - py)^2)))
  
  g1 <- ggplot(dataset, aes(x = X, y = Y)) + geom_point() +
        ggtitle(paste0("polynomial, deg = ", degr[i])) +
        geom_path(x = x[order(x[,1]),1], y = py[order(x[,1])], colour = "blue") 

  plot(g1)
  ggsave(paste0("SimpleRegCV", degr[i], ".pdf"), g1, width = 4, height = 4)
}

# random forest model
rf <- randomForest(y ~ ., data.frame(y, x), ntree = 1000)
py <- predict(rf)
res.is = rbind(res.is, data.frame(Model = "RF", MSE = mean((y - py)^2)))
g1 <- ggplot(dataset, aes(x = X, y = Y)) + geom_point() +
  ggtitle(paste0("Random Forest model")) +
  geom_path(x = x[order(x[,1]),1], y = py[order(x[,1])], colour = "blue") 

plot(g1)
ggsave(paste0("SimpleRegCVRF.pdf"), g1, width = 4, height = 4)

print(res.is)

# out-of-sample LOOCV MSE ------------------------------------------------------
res.oos = NULL
for (i in 1:length(degr))
{
  mse = c()
  for (j in 1:nrow(x))
  {
    tmp  <- data.frame(y = y, x[,1:degr[i]])
    lr   <- lm(y ~ ., data = tmp[-j,])# fit without the j-th sample
    pred <- predict(lr, newdata = tmp[j,])
    mse  <- c(mse, (tmp$y[j] - pred)^2)
  }
  res.oos = rbind(res.oos, data.frame(Model = degr[i], MSE = mean(mse)))
}


# random forest model
mse = c()
for (j in 1:nrow(x))
{
  tmp  <- data.frame(y = y, x[,1:degr[i]])
  rf   <- randomForest(y ~ ., data = tmp[-j,], ntree = 1000)
  pred <- predict(rf, newdata = tmp[j,])
  mse  <- c(mse, (tmp$y[j] - pred)^2)
}
res.oos = rbind(res.oos, data.frame(Model = "RF", MSE = mean(mse)))

print(res.oos)