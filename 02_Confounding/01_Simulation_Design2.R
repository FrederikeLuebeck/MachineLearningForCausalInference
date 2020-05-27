# Simulations on data drawn according to Design 2 in the presence of confounding
# Thesis: Table 2

library(ggplot2)
library(doParallel)
library(grf)
library(doSNOW)

source("00_FunctionSetup.R")

n.simulation = 100
d.values = c(4,6,8,10,12,14,16)


simulation_procedure <- function(d, form) {
  n <- 2000
  n.test <- 1000
  data <- design2(n,
                  n.test,
                  d,
                  noise = 0.5,
                  form = form)
  
  
  #causal forest with local centering
  cf_lc <- CF_estimator(data$X,
                        data$X.test,
                        data$Y,
                        data$W)
  cf_lc.evaluation <- evaluate(cf_lc$predictions, data$tau, cf_lc$sigma)
  
  # causal forest without local centering
  cf <- CF_estimator(data$X,
                     data$X.test,
                     data$Y,
                     data$W,
                     Y.hat = rep(0,n),
                     W.hat = rep(0,n))
  cf.evaluation <- evaluate(cf$predictions, data$tau, cf$sigma)
  
  data.frame(n = n,
             d = d,
             cf_lc.mse = cf_lc.evaluation$mse, 
             cf_lc.bias = cf_lc.evaluation$bias,
             cf_lc.sigma = mean(cf_lc$sigma),
             cf_lc.coverage = cf_lc.evaluation$coverage,
             cf.mse = cf.evaluation$mse, 
             cf.bias = cf.evaluation$bias,
             cf.sigma = mean(cf$sigma),
             cf.coverage = cf.evaluation$coverage)
}

columns <- c("n",
             "d",
             "cf_lc.mse",
             "cf_lc.bias",
             "cf_lc.sigma",
             "cf_lc.coverage",
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
  save.image(paste("02_Confounding/01_Design2_",form,".RData",sep=""))
}
