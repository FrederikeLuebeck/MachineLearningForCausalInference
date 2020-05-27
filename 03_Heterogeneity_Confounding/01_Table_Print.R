# Combination of Results to a Table

library(ggplot2)
library(tidyverse)
library(xtable)
library(tables)

# Noise = 1
load("01_n2000_noise15_nonlinearscaled.RData")
n_2000_1 <- result[c("n",
                     "d",
                     "noise",
                     "cf.mse",
                     "cf.bias",
                     "cf.sigma",
                     "cf.coverage",
                     "cf4000.mse",
                     "cf4000.bias",
                     "cf4000.sigma",
                     "cf4000.coverage")]

load("01_n5000_noise1_linear.RData")
n_5000_1 <- result[c("n",
                     "d",
                     "noise",
                     "cf.mse",
                     "cf.bias",
                     "cf.sigma",
                     "cf.coverage",
                     "cf4000.mse",
                     "cf4000.bias",
                     "cf4000.sigma",
                     "cf4000.coverage")]
load("01_n10000_noise1_linear.RData")
n_10000_1 <- result[c("n",
                      "d",
                      "noise",
                      "cf.mse",
                      "cf.bias",
                      "cf.sigma",
                      "cf.coverage",
                      "cf4000.mse",
                      "cf4000.bias",
                      "cf4000.sigma",
                      "cf4000.coverage")]
n_1 <- rbind(n_2000_1,
             n_5000_1,
             n_10000_1)

# Noise = 1.5

load("01_n2000_noise15_linear.RData")
n_2000_15 <- result[c("n",
                      "d",
                      "noise",
                      "cf.mse",
                      "cf.bias",
                      "cf.sigma",
                      "cf.coverage",
                      "cf4000.mse",
                      "cf4000.bias",
                      "cf4000.sigma",
                      "cf4000.coverage")]

load("01_n5000_noise15_linear.RData")
n_5000_15 <- result[c("n",
                      "d",
                      "noise",
                      "cf.mse",
                      "cf.bias",
                      "cf.sigma",
                      "cf.coverage",
                      "cf4000.mse",
                      "cf4000.bias",
                      "cf4000.sigma",
                      "cf4000.coverage")]
load("01_n10000_noise15_linear.RData")
n_10000_15 <- result[c("n",
                       "d",
                       "noise",
                       "cf.mse",
                       "cf.bias",
                       "cf.sigma",
                       "cf.coverage",
                       "cf4000.mse",
                       "cf4000.bias",
                       "cf4000.sigma",
                       "cf4000.coverage")]
n_15 <- rbind(n_2000_15,
              n_5000_15,
              n_10000_15)

linear2 <- rbind(n_1, n_15)

print(xtable(linear2, digits = 4), floating = F, include.rownames = FALSE)
