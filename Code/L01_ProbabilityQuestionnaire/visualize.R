dat <- read.table("../Datasets/questionnaire.csv", sep = ";", h = T)

library(ggplot2)
library(reshape2)
library(ggridges)

dat <- dat[,c(1, order(colMeans(dat[,-1])) + 1)]
cmn <- data.frame(ID = 1:(ncol(dat)-1), value = round(colMeans(dat[,-1]),1))

tmp <- melt(dat, id.vars = "ID")

g1 <- ggplot(tmp, aes(x = value, y = factor(variable))) + 
  geom_density_ridges(alpha = 0.8, fill = "cornflowerblue") + 
  xlab("probability (in %)") +
  ylab("expression") + geom_point() + geom_jitter(width = 1, height = 0.1) + 
  geom_vline(xintercept = 50, lty = "dotted") + 
  geom_text(data = cmn, aes(x = value, y = ID + 0.5, label = round(value,1)), colour = "red")

plot(g1)

ggsave("questionnaire.pdf", width = 11, height = 7)