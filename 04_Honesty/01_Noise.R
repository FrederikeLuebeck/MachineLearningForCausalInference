# Simulation on data drawn according to Design 4
# This is to explore the concept of honesty by comparing honest causal forests with their adaptive analog
# Parameter variance of noise varies, while all other parameters are constant
# Thesis: Figure 7


library(doParallel)
library(grf)
library(doSNOW)

source("00_FunctionSetup.R")

n.simulation = 100
noise.values = c(0, 0.005, 0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.8, 1, 1.1, 1.3, 1.5)


simulation_procedure <- function(noise, form) {
  n <- 2000
  n.test <- 1000
  d <- 6
  data <- design4(n,
                  n.test,
                  d,
                  e = 0.5,
                  form = form)
  
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
  
  data.frame(n = n,
             d = d,
             noise = noise,
             honest.mse = honest.evaluation$mse, 
             honest.bias = honest.evaluation$bias,
             honest.sigma = mean(honest$sigma),
             honest.coverage = honest.evaluation$coverage,
             adaptive.mse = adaptive.evaluation$mse, 
             adaptive.bias = adaptive.evaluation$bias,
             adaptive.sigma = mean(adaptive$sigma),
             adaptive.coverage = adaptive.evaluation$coverage)
}

columns <- c("n",
             "d",
             "noise",
             "honest.mse",
             "honest.bias",
             "honest.sigma",
             "honest.coverage",
             "adaptive.mse",
             "adaptive.bias",
             "adaptive.sigma",
             "adaptive.coverage")

for (form in c("linear", "nonlinear")) {
  result <- run_simulation(n.simulation = n.simulation,
                           parameter.values = noise.values,
                           procedure = simulation_procedure,
                           columns = columns,
                           form = form)
  save.image(paste("04_Honesty/01_Noise_",form,".RData",sep=""))
}
