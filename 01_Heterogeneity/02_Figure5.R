# Simulation on data drawn according to Design 1 in the presence of Strong Treatment Effect Heterogeneity
# Visualization of the specific predictions of one causal forest
# Thesis: Figure 5

library(grf)
library(doParallel)
library(doSNOW)
library(doRNG)
library(FNN)
library(ggplot2)
library(tidyverse)



source("00_FunctionSetup.R")

simulation_procedure <- function(d, form) {
  n <- 5000
  n.test <- 8000
  noise <- 0.5
  data <- design1(n,
                  n.test,
                  d,
                  e = 0.5,
                  noise = 0.5,
                  form = form)
  
  cf <- CF_estimator(data$X,
                     data$X.test,
                     data$Y,
                     data$W,)
  cf.evaluation <- evaluate(cf$predictions, data$tau, cf$sigma)
  
  # Nearest neighbor matching with k = 8, 15, 30
  knn8 <- NN_estimate(data$X,
                      data$X.test,
                      data$Y,
                      data$W,
                      k = 8)
  knn8.evaluation <- evaluate(knn8$predictions, data$tau, knn8$sigma)
  
  knn15 <- NN_estimate(data$X,
                       data$X.test,
                       data$Y,
                       data$W,
                       k = 15)
  knn15.evaluation <- evaluate(knn15$predictions, data$tau, knn15$sigma)
  
  knn30 <- NN_estimate(data$X,
                       data$X.test,
                       data$Y,
                       data$W,
                       k = 30)
  knn30.evaluation <- evaluate(knn30$predictions, data$tau, knn30$sigma)
  
  return(data.frame(n = n,
                    d = d,
                    X1 = data$X.test[,1],
                    X2 = data$X.test[,2],
                    X3 = data$X.test[,3],
                    tau = data$tau,
                    cf.mse = cf.evaluation$mse, 
                    cf.bias = cf.evaluation$bias,
                    cf.sigma = mean(cf$sigma),
                    cf.coverage = cf.evaluation$coverage,
                    cf.predictions = cf$predictions,
                    knn8.mse = knn8.evaluation$mse, 
                    knn8.bias = knn8.evaluation$bias,
                    knn8.sigma = mean(knn8$sigma),
                    knn8.coverage = knn8.evaluation$coverage,
                    knn8.predictions = knn8$predictions,
                    knn15.mse = knn15.evaluation$mse, 
                    knn15.bias = knn15.evaluation$bias,
                    knn15.sigma = mean(knn15$sigma),
                    knn15.coverage = knn15.evaluation$coverage,
                    knn15.predictions = knn15$predictions,
                    knn30.mse = knn30.evaluation$mse, 
                    knn30.bias = knn30.evaluation$bias,
                    knn30.sigma = mean(knn30$sigma),
                    knn30.coverage = knn30.evaluation$coverage,
                    knn30.predictions = knn30$predictions))
  
}

# Run simulation for form = "linear"
result <- simulation_procedure(d=10, form="linear")

df <- result %>%
  select(X1,
         X2,
         tau,
         cf.predictions,
         knn30.predictions) %>%
  gather(key="estimator", value="estimate", -X1, -X2) 

df$estimator <- factor(df$estimator,
                       levels = c("tau", "cf.predictions", "knn30.predictions"),
                       labels = c("True Effect", "Causal Forest", "30-NN"))

ggplot(df, aes(X1,X2)) +
  geom_point(aes(color = estimate),size=0.7) +
  scale_colour_gradient2(low="yellow",mid="turquoise3", high="blue", midpoint=1.5) +
  labs(x="X1", y="X2", col="Tau") +
  theme(panel.background = element_rect(fill="white",colour="grey"), text = element_text(size=7),  legend.title = element_blank(), aspect.ratio = 1) +
  facet_wrap( ~ estimator, nrow=1)

ggsave("01_Heterogeneity/02_Figure5_facet_linear.png", width=19, height = 7, units = "cm", dpi=300)

# Run simulation for form = "nonlinear"
result <- simulation_procedure(d=10, form="nonlinear")

df <- result %>%
  select(X1,
         X2,
         tau,
         cf.predictions,
         knn30.predictions) %>%
  gather(key="estimator", value="estimate", -X1, -X2) 

df$estimator <- factor(df$estimator,
                       levels = c("tau", "cf.predictions", "knn30.predictions"),
                       labels = c("True Effect", "Causal Forest", "30-NN"))

ggplot(df, aes(X1,X2)) +
  geom_point(aes(color = estimate),size=0.7) +
  scale_colour_gradient2(low="yellow",mid="turquoise3", high="blue", midpoint=2.5) +
  labs(x="X1", y="X2", col="Tau") +
  theme(panel.background = element_rect(fill="white",colour="grey"), text = element_text(size=7),  legend.title = element_blank(), aspect.ratio = 1) +
  facet_wrap( ~ estimator, nrow=1)

ggsave("01_Heterogeneity/02_Figure5_facet_nonlinear.png", width=19, height = 7, units = "cm", dpi=300)

