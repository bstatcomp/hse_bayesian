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
                  m = max(tmp$ID),
                  weight = tmp$Weight,
                  id= tmp$ID)

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
#samples <- sampling(ttest_compiled, data = stan_data, verbose = F,
#                  chains = 1, iter = 1200, warmup = 200, seed = 0, refresh = 0)