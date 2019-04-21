library(rstan)
library(reshape2)

# load and prepare data
dat <- read.csv("../Code/Datasets/poland.dat", sep = "\t", h = F)
print(dat)

tmp <- melt(dat)
tmp <- tmp[complete.cases((tmp)),]
names(tmp) <- c("ID", "Weight")
tmp$ID <- as.numeric(tmp$ID)

stan_data <- list(n = nrow(tmp),
                  k = max(tmp$ID),
                  weight = tmp$Weight,
                  id= tmp$ID,
                  mu_min = 0,
                  mu_max = 10)

# standard one-way ANOVA
summary(aov(Weight ~ ID, data = tmp))

# load or compile model, if necessary
if (!file.exists("./Temp/one-way_anova.compiled")) {
  anova_compiled <- stan_model("one-way_anova.stan")
  saveRDS(anova_compiled, file = "./Temp/one-way_anova.compiled")
} else {
  anova_compiled <- readRDS("./Temp/one-way_anova.compiled")  
}

# sample from model
samples <- sampling(anova_compiled, data = stan_data,
                  chains = 1, iter = 1200, warmup = 200, seed = 0)

mu <- extract(samples)$mu
winner <- array(0, dim = 8)

for (j in 1:nrow(mu)) {
  x <- mu[j,]
  winner[order(-x)[1]] <- winner[order(-x)[1]] + 1 
}

winner / 1000

