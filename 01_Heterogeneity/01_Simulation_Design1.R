# Simulation on data drawn according to Design 1 in the presence of Strong Treatment Effect Heterogeneity
# This is to explore the ability of causal forests to detect heterogeneity for varying dimensions of the feature space (d)
# Thesis: Figure 3

library(grf)
library(doParallel)
library(doSNOW)
library(doRNG)
library(FNN)

source("00_FunctionSetup.R")

n.simulation <- 100
d.values <- c(4,6,8,12,20,28,36,40)

simulation_procedure <- function(d, form) {
  n <- 4000
  n.test <- 1000
  noise <- 0.5
  data <- design1(n,
                  n.test,
                  d,
                  e = 0.5,
                  noise = noise,
                  form = form)
  # causal forest  
  cf <- CF_estimator(data$X,
                     data$X.test,
                     data$Y,
                     data$W,
                     num.trees = 4000)
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
  
  data.frame(n = n,
             d = d,
             cf.mse = cf.evaluation$mse, 
             cf.bias = cf.evaluation$bias,
             cf.sigma = mean(cf$sigma),
             cf.coverage = cf.evaluation$coverage,
             knn8.mse = knn8.evaluation$mse, 
             knn8.bias = knn8.evaluation$bias,
             knn8.sigma = mean(knn8$sigma),
             knn8.coverage = knn8.evaluation$coverage,
             knn15.mse = knn15.evaluation$mse, 
             knn15.bias = knn15.evaluation$bias,
             knn15.sigma = mean(knn15$sigma),
             knn15.coverage = knn15.evaluation$coverage,
             knn30.mse = knn30.evaluation$mse, 
             knn30.bias = knn30.evaluation$bias,
             knn30.sigma = mean(knn30$sigma),
             knn30.coverage = knn30.evaluation$coverage)
  
}
columns = c("n",
            "d",
            "cf.mse",
            "cf.bias",
            "cf.sigma",
            "cf.coverage",
            "knn8.mse",
            "knn8.bias",
            "knn8.sigma",
            "knn8.coverage",
            "knn15.mse",
            "knn15.bias",
            "knn15.sigma",
            "knn15.coverage",
            "knn30.mse",
            "knn30.bias",
            "knn30.sigma",
            "knn30.coverage")

# Running the simulation

for (form in c("linear", "nonlinear")) {
  result <- run_simulation(n.simulation = n.simulation,
                           parameter.values = d.values,
                           procedure = simulation_procedure,
                           columns = columns,
                           form = form)
  save.image(paste("01_Heterogeneity/01_Simulation_Design1_",form,".RData",sep=""))
}

