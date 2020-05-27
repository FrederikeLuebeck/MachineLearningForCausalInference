# This simulation examines a Causal Foersts ability to handle both challenges, heterogeneity and confounding
# Thesis: Table 3

library(ggplot2)
library(doParallel)
library(grf)
library(doSNOW)

source("00_FunctionSetup.R")

n.simulation = 100
d.values = c(6,20)


simulation_procedure <- function(d, form) {
  n <- 2000
  n.test <- 1000
  noise <- 1.5 # vary noise (1, 1.5)
  data <- design3(n,
                  n.test,
                  d,
                  noise = noise,
                  form = form)
  
  # causal forest with num.tree = 2000
  cf <- CF_estimator(data$X,
                     data$X.test,
                     data$Y,
                     data$W)
  cf.evaluation <- evaluate(cf$predictions, data$tau, cf$sigma)
  
  # causal forest with num.tree = 4000
  cf4000 <- CF_estimator(data$X,
                         data$X.test,
                         data$Y,
                         data$W,
                         num.tree = 4000)
  cf4000.evaluation <- evaluate(cf4000$predictions, data$tau, cf4000$sigma)
  
  data.frame(n = n,
             d = d,
             noise = noise,
             cf.mse = cf.evaluation$mse, 
             cf.bias = cf.evaluation$bias,
             cf.sigma = mean(cf$sigma),
             cf.coverage = cf.evaluation$coverage,
             cf4000.mse = cf4000.evaluation$mse, 
             cf4000.bias = cf4000.evaluation$bias,
             cf4000.sigma = mean(cf4000$sigma),
             cf4000.coverage = cf4000.evaluation$coverage)
}

columns <- c("n",
             "d",
             "noise",
             "cf.mse",
             "cf.bias",
             "cf.sigma",
             "cf.coverage",
             "cf4000.mse",
             "cf4000.bias",
             "cf4000.sigma",
             "cf4000.coverage"
)


for (form in c("nonlinear")) {
  result <- run_simulation(n.simulation = n.simulation,
                           parameter.values = d.values,
                           procedure = simulation_procedure,
                           columns = columns,
                           form = form)
  save.image(paste("04_n2000_noise15_",form,".RData",sep=""))
}
