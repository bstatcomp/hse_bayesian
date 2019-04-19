library(rstan)

# generate toy data
set.seed(1)
n <- 50
x <- rnorm(n)
y <- 2.5 * x + 1 + rnorm(n)
x <- c(x, -4, 3, 4) # outliers
y <- c(y, 3, 1, 2)
plot(x,y)

# OLS/ML
line <- lm(y ~ x)
print(line)
plot(x,y)
abline(line, col = "red")

stan_data <- list(n = length(y),
                  y = y,
                  x = x)

# load or compile model, if necessary
if (!file.exists("./Temp/slr.compiled")) {
  slr_compiled <- stan_model("simple_linear_regression.stan")
  saveRDS(slr_compiled, file = "./Temp/slr.compiled")
} else {
  slr_compiled <- readRDS("./Temp/slr.compiled")  
}

# sample from model
samples <- sampling(slr_compiled, data = stan_data,
                  chains = 1, iter = 1200, warmup = 200, seed = 0)