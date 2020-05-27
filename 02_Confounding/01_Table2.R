# Simulations on data drawn according to Design 2 in the presence of confounding
# Visualization of results

library(ggplot2)
library(tidyverse)
library(xtable)

load("02_Confounding/01_Design2_linear.RData")

lin <- result[c("n",
                "d",
                "cf_lc.mse",
                "cf_lc.bias",
                "cf_lc.coverage",
                "cf.mse",
                "cf.bias",
                "cf.coverage")]

load("02_Confounding/01_Design2_nonlinear.RData")
nonlin <- result[c("cf_lc.mse",
                   "cf_lc.bias",
                   "cf_lc.coverage",
                   "cf.mse",
                   "cf.bias",
                   "cf.coverage")]

d <- cbind(lin, nonlin)

xtable(lin, type=latex, digits = 3)