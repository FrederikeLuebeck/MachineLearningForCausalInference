# Simulation on data drawn according to Design 1 in the presence of Strong Treatment Effect Heterogeneity
# Thesis: Table 1

library(grf)
library(doParallel)
library(doSNOW)
library(doRNG)
library(FNN)

source("00_FunctionSetup.R")

n.simulation <- 100
d.values <- c(6,10,20)

simulation_procedure <- function(d, form) {
  n <- 2000 # vary parameter n (2000, 4000, 8000)
  n.test <- 1000
  noise <- 0.8
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
  
  data.frame(n = n,
             d = d,
             cf.mse = cf.evaluation$mse, 
             cf.bias = cf.evaluation$bias,
             cf.sigma = mean(cf$sigma),
             cf.coverage = cf.evaluation$coverage)
  
}
columns = c("n",
            "d",
            "cf.mse",
            "cf.bias",
            "cf.sigma",
            "cf.coverage")

for (form in c("linear", "nonlinear")) {
  result <- run_simulation(n.simulation = n.simulation,
                           parameter.values = d.values,
                           procedure = simulation_procedure,
                           columns = columns,
                           form = form)
  save.image(paste("01_Heterogeneity/04_Table_",form,"_2000",".RData",sep=""))
}

