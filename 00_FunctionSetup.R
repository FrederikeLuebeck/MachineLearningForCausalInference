# This script contains the basic functions used in the simulations
# Part 1: Data Generating Processes (Design 1 to 4) 
# Part 2: Functions for Performance Evaluation
# Part 3: Estimators
# Part 4: Parallelization

library(grf)
library(doParallel)
library(doSNOW)
library(doRNG)
library(FNN)

# Part 1: Data Generating Processes (Design 1 to 4)
# basic functions

nonlinear <- function(x) {
  1 + 1 / (1 + exp(-20 * (x - 1/3)))
}
propensity <- function(x) {
  0.25 + dbeta(x,2,4) / 4
}

# Design 1: Strong Treatment Effect Heterogeneity (lineaer and nonlinear)

design1 <- function(n,
                    n.test,
                    d,
                    e,
                    noise,
                    form) {
  X <- matrix(runif(n * d, 0, 1), n, d)
  X.test = matrix(runif(n.test * d, 0, 1), n.test, d)
  W <- rbinom(n, 1, e)
  
  if (form == "linear") {
    Y <- (2*X[,1] + X[,2]) * W + X[,2] + X[,3] + rnorm(n, 0, noise)
    tau <- 2*X.test[,1] + X.test[,2]
  }
  
  if (form == "nonlinear") {
    Y <- sapply(X[,1], nonlinear) * sapply(X[,2], nonlinear) * W + sapply(X[,2], nonlinear) * sapply(X[,3], nonlinear) + rnorm(n, 0, noise)
    tau <- sapply(X.test[,1], nonlinear) * sapply(X.test[,2], nonlinear)
    
  }
  return(list(X=X,
              X.test=X.test,
              W=W,
              Y=Y,
              tau=tau))
}

# Design 2: Confounding (linear and nonlinear)

design2 <- function(n,
                    n.test,
                    d,
                    noise,
                    form) {
  X <- matrix(runif(n * d, 0, 1), n, d)
  X.test = matrix(runif(n.test * d, 0, 1), n.test, d)
  W <- rbinom(n, size = 1, p = sapply(X[,1], propensity))
  
  if (form == "linear") {
    Y <- 2 * X[,1] + rnorm(n, 0, noise)
  }
  
  if (form == "nonlinear") {
    Y <- sapply(X[,1], nonlinear) + rnorm(n, 0, noise)
  }
  tau <- rep(0,n)
  
  return(list(X=X,
              X.test=X.test,
              W=W,
              Y=Y,
              tau=tau))
}

# Design 3: Heterogeneity & Confounding

design3 <- function(n,
                    n.test,
                    d,
                    noise,
                    form) {
  X <- matrix(runif(n * d, 0, 1), n, d)
  X.test = matrix(runif(n.test * d, 0, 1), n.test, d)
  W <- rbinom(n, size = 1, p = sapply(X[,1], propensity))
  
  if (form == "linear") {
    Y <- (2*(1-X[,1]) + X[,2]) * W + X[,1] + X[,3] + rnorm(n, 0, noise)
    tau <- (2*(1-X.test[,1]) + X.test[,2])
  }
  
  if (form == "nonlinear") {
    Y <-  2*(2-sapply(X[,1], nonlinear)) * sapply(X[,2], nonlinear) * W + sapply(X[,1], nonlinear) * sapply(X[,3], nonlinear) + rnorm(n, 0, noise)
    tau <-  1.5*(2-sapply(X.test[,1], nonlinear)) * sapply(X.test[,2], nonlinear)
  }
  
  return(list(X=X,
              X.test=X.test,
              W=W,
              Y=Y,
              tau=tau))
}

# Design 4: Honesty

design4 <- function(n,
                    n.test,
                    d,
                    e,
                    form) {
  X <- matrix(runif(n * d, 0, 1), n, d)
  X.test = matrix(runif(n.test * d, 0, 1), n.test, d)
  W <- rbinom(n, 1, e)
  
  if (form == "linear") {
    Y <- 2*X[,1]*W + X[,2] 
    tau <- 2*X.test[,1]
  }
  
  if (form == "nonlinear") {
    Y <- sapply(X[,1], nonlinear) * W + sapply(X[,3], nonlinear) 
    tau <- sapply(X.test[,1], nonlinear)
  }
  
  if (form == "sinus") {
    Y <- sapply(X[,1], sinus) * W
    tau <- sapply(X.test[,1], sinus)
  }
  
  return(list(X=X,
              X.test=X.test,
              W=W,
              Y=Y,
              tau=tau))
}

# Part 2: Performance Evaluation

mse <- function(predictions, true) {
  return(mean((predictions-true)^2))
}

bias <- function(predictions, true){
  return(abs(mean(predictions-true)))
}

covered <- function(predictions, true, sigma) {
  return(mean(abs(predictions-true) / sigma <= 1.96))
}

evaluate <- function(predictions, true, sigma) {
  return(list(mse = mse(predictions,true),
              bias = bias(predictions,true),
              coverage = covered(predictions,true,sigma)))
}

# Part 3: Estimators

# Causal Forest with user-specific parameters
# Honesty can be disabled by setting 'honest = FALSE'
# Local centering can be disabled by setting Y.hat and W.hat to vectors of zeros

CF_estimator <- function(X,
                         X.test,
                         Y,
                         W,
                         Y.hat = NULL,
                         W.hat = NULL,
                         num.trees = 2000,
                         clusters = NULL,
                         honesty = TRUE,
                         honesty.fraction = 0.5,
                         honesty.prune.leaves = TRUE,
                         min.node.size = 5) {
  CF <- causal_forest(X,
                      Y,
                      W,
                      Y.hat = Y.hat,
                      W.hat = W.hat,
                      num.trees = num.trees,
                      clusters = clusters,
                      min.node.size = min.node.size,
                      honesty = honesty,
                      honesty.fraction = honesty.fraction,
                      honesty.prune.leaves = honesty.prune.leaves,
                      seed = 1)
  
  estimates <- predict(CF,
                       X.test,
                       estimate.variance = TRUE)
  ate <- average_treatment_effect(CF, target.sample = "all")
  
  return(list(predictions = estimates$predictions,
              sigma = sqrt(estimates$variance.estimates),
              ate = ate,
              var.imp = variable_importance(CF)))
}

# KNN

NN_estimate <- function(X,
                        X.test,
                        Y,
                        W,
                        k) {
  
  knn.control <- knn.reg(X[W==0,], X.test, Y[W==0], k = k)$pred # outcome of control units
  knn.treated <- knn.reg(X[W==1,], X.test, Y[W==1], k = k)$pred # outcome of treated units
  
  # treatment effect
  knn.tau <- knn.treated - knn.control
  
  # estimating the variance
  knn.control2 <- knn.reg(X[W==0,], X.test, Y[W==0]^2, k = k)$pred
  knn.treated2 <- knn.reg(X[W==1,], X.test, Y[W==1]^2, k = k)$pred
  knn.control.var <- (knn.control2 - knn.control^2) / (k - 1)
  knn.treated.var <- (knn.treated2 - knn.treated^2) / (k - 1)
  
  return(list(predictions = knn.tau,
              sigma = sqrt(knn.control.var + knn.treated.var),
              ate = mean(knn.tau)))
}

# Part 4: Parallelization

# This function runs the user-specific 'procedure' on a parallel backend
# 'n.simulation' is the number each simulation is repeated
# 'parameter.values' is a vector of the parameters to iterate over
# 'procedure' is the function that is executed in each "foreach" iteration
# 'columns' is a list of the column names, needed to return a correct and readable dataframe of the results
# 'form' is either 'linear' or 'nonlinear', used for the DGP


run_simulation <- function(n.simulation,
                           parameter.values,
                           procedure,
                           columns,
                           form) {
  # parallel backend
  cores=detectCores()
  cl <- makeCluster(cores[1]-1)
  registerDoSNOW(cl)
  
  # progess bar
  pb <- txtProgressBar(max=n.simulation, style=3)
  progress <- function(n) {
    setTxtProgressBar(pb, n)
  }
  opts <- list(progress=progress)
  
  # initialize dataframe
  output <- setNames(data.frame(matrix(ncol = length(columns), nrow = 0)),
                     columns)
  
  # run simulation
  # using the doRNG package to guarantee reproducible results
  set.seed(1)
  for(parameter in parameter.values){
    print(paste("Running ",parameter, ":"))
    results = foreach(i=1:n.simulation,
                      .combine=rbind,
                      .options.snow=opts,
                      .packages=c('grf', 'FNN')) %dorng% {
                        source("00_FunctionSetup.R", local = TRUE)
                        procedure(parameter, form)
                      }
    output <- rbind.data.frame(output, colMeans(results))
  }
  
  # stop parallelization and progress bar
  stopCluster(cl)
  close(pb)
  
  output <- setNames(output, columns)
  
  return(output)
}