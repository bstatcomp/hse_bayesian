library(ggplot2)
library(rstan)
library(reshape2)

# Load and prepare data --------------------------------------------------------
dataset <- read.table("../datasets/poland.dat", sep = "\t", h = F)

# Not all pigs have the same number of piglets, however, Stan does not support 
# ragged arrays; see Stan manual: ragged data structures.
# There are (at least) two ways of dealing with this:
# - Send the entire 2D array + 1D array with lengths
# - Flatten the data into a 1D array and add a 1D array of group membership.
# We'll do the latter.

y <- melt(dataset)
y$variable <- as.numeric(as.factor(y$variable)) # number pigs
y <- y[!is.na(y$value),]

res <- NULL

stan_data <- list(y  = y$value,
                  g  = y$variable,
                  ng = 8,
                  n  = nrow(y))


# Model 1 ----------------------------------------------------------------------
model_fit <- stan_model(file = "piglets_model01.stan")

smp01 <- sampling(model_fit,  data = stan_data, 
                iter = 1500, warmup = 500, chains = 1, verbose = T, 
                seed = 1)

tmp <- summary(smp01)$summary[2,]
res <- rbind(res, data.frame(Model = "Model 1",
                             Group = 1:8, 
                             Mean  = tmp[1],
                             Upper = tmp[4],
                             Lower = tmp[8]))


# Model 2 ----------------------------------------------------------------------
model_fit <- stan_model(file = "piglets_model02.stan")

smp02 <- sampling(model_fit,  data = stan_data, 
              iter = 1500, warmup = 500, chains = 1, verbose = T, 
              seed = 1)

tmp <- summary(smp02)$summary[2:9,]

res <- rbind(res, data.frame(Model = "Model 2",
                             Group = 1:8, 
                             Mean  = tmp[,1],
                             Upper = tmp[,4],
                             Lower = tmp[,8]))


# Model 3 ----------------------------------------------------------------------
model_fit <- stan_model(file = "piglets_model03.stan")

smp03 <- sampling(model_fit,  data = stan_data, 
              iter = 1500, warmup = 500, chains = 1, verbose = T, 
              seed = 1)

tmp <- summary(smp03)$summary[4:11,]

res <- rbind(res, data.frame(Model = "Model 3",
                             Group = 1:8, 
                             Mean  = tmp[,1],
                             Upper = tmp[,4],
                             Lower = tmp[,8]))

# Visualize --------------------------------------------------------------------

g1 <- ggplot(res, aes(x = Group, y = Mean, ymin = Lower, ymax = Upper,
                      group = Model, colour = Model)) +
  geom_point(position=position_dodge(width=0.3)) + 
  geom_errorbar(position=position_dodge(width=0.3)) + 
  coord_cartesian(ylim=c(1.5, 4))
ggsave(g1, file = "piglets_compare.pdf", width = 6, height = 4)