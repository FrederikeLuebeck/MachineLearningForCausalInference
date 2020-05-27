# Simulation on data drawn according to Design 1 in the presence of Strong Treatment Effect Heterogeneity
# Thesis: Table 1
# combine results for n = 2000,4000,8000 and form=linear, nonlinear

load("01_Heterogeneity/04_Table_linear_2000.RData")
lin2 <- result[c("n",
                 "d",
                 "cf.mse",
                 "cf.bias",
                 "cf.coverage")]
load("01_Heterogeneity/04_Table_nonlinear_2000.RData")
nonlin2 <- result[c("cf.mse",
                    "cf.bias",
                    "cf.coverage")]

d2 <- cbind(lin2,nonlin2)

load("01_Heterogeneity/04_Table_linear_4000.RData")
lin4 <- result[c("n",
                 "d",
                 "cf.mse",
                 "cf.bias",
                 "cf.coverage")]
load("01_Heterogeneity/04_Table_nonlinear_4000.RData")
nonlin4 <- result[c("cf.mse",
                    "cf.bias",
                    "cf.coverage")]

d4 <- cbind(lin4,nonlin4)

load("01_Heterogeneity/04_Table_linear_8000.RData")
lin8 <- result[c("n",
                 "d",
                 "cf.mse",
                 "cf.bias",
                 "cf.coverage")]
load("01_Heterogeneity/04_Table_nonlinear_8000.RData")
nonlin8 <- result[c("cf.mse",
                    "cf.bias",
                    "cf.coverage")]

d8 <- cbind(lin8,nonlin8)

d <- rbind(d2,d4,d8)


xtable(d, type=latex, digits = 4)