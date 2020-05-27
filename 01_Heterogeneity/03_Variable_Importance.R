# Simulation on data drawn according to Design 1 in the presence of Strong Treatment Effect Heterogeneity
# Variable Importance

library(ggplot2)
library(doParallel)
library(grf)
library(doSNOW)

source("00_FunctionSetup.R")

n.simulation = 100
d.values = c(8)


simulation_procedure <- function(d, form) {
  n <- 2000
  n.test <- 1000
  data <- design1(n,
                  n.test,
                  d,
                  e = 0.5,
                  noise = 0.5,
                  form = form)
  
  
  # causal forest with local centering
  cf <- CF_estimator(data$X,
                     data$X.test,
                     data$Y,
                     data$W)
  cf.evaluation <- evaluate(cf$predictions, data$tau, cf$sigma)

  data.frame(n = n,
             d = d,
             cf.mse = cf.evaluation$mse,
             cf.varimp = t(cf$var.imp))
}

columns <- c("n",
             "d",
             "cf.mse",
             "cf.varimp.1",
             "cf.varimp.2",
             "cf.varimp.3",
             "cf.varimp.4",
             "cf.varimp.5",
             "cf.varimp.6",
             "cf.varimp.7",
             "cf.varimp.8")


for (form in c("linear", "nonlinear")) {
  result <- run_simulation(n.simulation = n.simulation,
                           parameter.values = d.values,
                           procedure = simulation_procedure,
                           columns = columns,
                           form = form)
  save.image(paste("01_Heterogeneity/03_VarImp_",form,".RData",sep=""))
}
