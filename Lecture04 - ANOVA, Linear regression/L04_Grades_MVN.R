# Using a Bayesian MVN Model on a student score dataset ------------------------

library(ggplot2)
library(rstan)
library(reshape2)

# Load and visualize data ------------------------------------------------------

dataset <- read.table("../Code/Datasets/studentScores.csv", sep = ",", h = T)
tmp <- melt(dataset)
names(tmp) <- c("Course", "Score")
g1 <- ggplot(tmp, aes(x = Course, y = Score, fill = Course)) + geom_boxplot() 
#ggsave("StudentScoresVis01.pdf", g1, width = 6, height = 3.5)
plot(g1)

# Modelling --------------------------------------------------------------------
model_fit <- stan_model(file = "./mvn_model.stan")

stan_data <- list(y = dataset,
                  n = nrow(dataset), 
                  d = ncol(dataset))

samples <- sampling(model_fit,  data = stan_data, 
                iter = 5500, warmup = 500, chains = 1, seed = 0)

print(samples, par = c("mu", "corr"))


plot(samples, par = c("mu"))
plot(samples, par = c("corr"))
