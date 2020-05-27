# Visualization of specific predictions of a Causal Forest on data drawn according to design 4


library(doParallel)
library(grf)
library(doSNOW)
library(tidyverse)
library(ggplot2)

source("00_FunctionSetup.R")

set.seed(1)

n <- 4000
n.test <- 1000
d <- 6
noise <- 0.5

data <- design4(n,
                n.test,
                d,
                e = 0.5,
                form = "nonlinear")
data$Y <- data$Y + rnorm(n, 0, noise)

honest <- CF_estimator(data$X,
                       data$X.test,
                       data$Y,
                       data$W,
                       honesty.fraction = 0.5,
                       honesty.prune.leaves = TRUE)
honest.evaluation <- evaluate(honest$predictions, data$tau, honest$sigma)

adaptive <- CF_estimator(data$X,
                         data$X.test,
                         data$Y,
                         data$W,
                         honesty = FALSE)
adaptive.evaluation <- evaluate(adaptive$predictions, data$tau, adaptive$sigma)

# Visualization of Results

predictions <- data.frame(X1 = data$X.test[,1],
                          true = data$tau,
                          honest = honest$predictions,
                          adaptive = adaptive$predictions)
df <- predictions %>%
  select(X1, true, honest, adaptive) %>%
  gather(key="method", value="value", -X1)

ggplot(df, aes(x=X1, y=value, color=method)) +
  geom_point(size=0.4) +
  scale_color_manual(values = c("brown1", "turquoise3", "grey65"), labels=c("Adaptive", "Honest", "True Effect")) +
  theme(panel.background = element_rect(fill="white",colour="grey"), text = element_text(size=18), legend.title = element_blank(), legend.position = "top") +
  labs(y="Treatment Effect", x="X1")

ggsave("02_Figure7b.png", width=17, height = 11, units = "cm")