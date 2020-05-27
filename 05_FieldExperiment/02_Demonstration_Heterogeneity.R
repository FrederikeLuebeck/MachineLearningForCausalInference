library(foreign)
library(sandwich)
library(lmtest)
library(grf)
library(ggplot2)
library(tidyverse)

library(causalToolbox)

source("05_FieldExperiment/01_DataSetup.R")

# t = 1, transgender tolerance index

idx <- trans.tolerance.dv.t1
include.obs <- !is.na(idx)

cf<- causal_forest(X=x[include.obs,],
                   Y=idx[include.obs],
                   W=data$treat_ind[include.obs],
                   num.trees = 4000,
                   clusters = data$hh_id_numeric[include.obs],
                   seed = 1)

ate <-average_treatment_effect(cf)
ate_estimate <- ate[1]
ci_low <- ate[1]-qnorm(0.975)*ate[2]
ci_high <- ate[1]+qnorm(0.975)*ate[2]

paste("95% CI: ", ate[1], "+/-",  qnorm(0.975)*ate[2])

# histogram of treatment effect estimates 

tau.hat <- predict(cf, estimate.variance = TRUE)
cates <- data.frame(pred=tau.hat$predictions)


ggplot(cates, aes(x=pred)) +
  geom_histogram(color="white", fill="turquoise3", bins=40) +
  geom_vline(aes(xintercept=ate_estimate), color="brown1", size=1) +
  geom_vline(aes(xintercept=ci_low), color="brown1", size=0.5) +
  geom_vline(aes(xintercept=ci_high), color="brown1", size=0.5) +
  theme(panel.background = element_rect(fill="white",colour="grey"), text = element_text(size=14)) +
  labs(x="Treatment Effect Estimates")

ggsave("05_FieldExperiment/Histogram_CI.png", width=17, height = 12, units = "cm")

# plot estimated values with confidence intervals

plot_CI <- function(cf_preds, z = qnorm(0.975)) {
  out <- ggplot(mapping = aes(x = rank(cf_preds$predictions), y = cf_preds$predictions)) +
    geom_errorbar(mapping = aes(ymin = cf_preds$predictions + z * sqrt(cf_preds$variance.estimates), ymax = cf_preds$predictions - z * sqrt(cf_preds$variance.estimates)), color="grey",size=0.3) +
    geom_point(size=0.5, col="brown1") +
    labs(x = "Individuals, ordered", y = "Estimated Treatment Effect") +
    theme(panel.background = element_rect(fill="white",colour="grey"), text = element_text(size=14)) +
    ylim(-0.4,0.8)
  
  return(out)
}

plot_CI(tau.hat)

ggsave("05_FieldExperiment/CI.png", width=17, height = 12, units = "cm")

# variable importance plot

varimp <- data.frame(imp=variable_importance(cf), cols=colnames(x))
print(xtable(t(varimp[order(-varimp$imp),][1:5,]), digits=2))

ggplot(varimp, aes(y=imp, x=cols)) +
  geom_bar(stat="identity") +
  theme(panel.background = element_rect(fill="white",colour="grey"), text = element_text(size=22), axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y="Variable Importance", x=element_blank()) 

#ggsave("Varimp.png")

# Visualization of Relationship between CATE and variable age
plot.df <- data.frame(age=x[include.obs,"vf_age"],cate=tau.hat$predictions)

ggplot(plot.df, aes(x=age,y=cate)) +
  geom_point() +
  theme(panel.background = element_rect(fill="white",colour="grey"), text = element_text(size=28), legend.position = "top") +
  xlab("Age") + ylab("CATE")

#ggsave("05_FieldExperiment/age.png", height = 15, width = 25, units="cm")


ate_old <- average_treatment_effect(cf, subset=x[include.obs,"vf_age"] > 40)
ate_young <- average_treatment_effect(cf, subset=x[include.obs,"vf_age"] <= 40)

n_old <- length(idx[include.obs][x[include.obs,"vf_age"] > 40])
n_young <- length(idx[include.obs][x[include.obs,"vf_age"] <= 40])

# difference in ate
paste("CI", round(ate_old[1]-ate_young[1],3), "+/-",  round(qnorm(0.975)*sqrt(ate_old[2]^2+ate_young[2]^2),3))

plot.df <-data.frame(ate=c(ate_old[1], ate_young[1]), se=c(ate_old[2], ate_young[2]), group=c("old","young"))

ggplot(plot.df, aes(x=1, y=ate, color=group)) +
  geom_point(size=4, position=position_dodge(width=0.3)) +
  geom_errorbar(aes(ymin=ate-qnorm(0.975)*se, ymax=ate+qnorm(0.975)*se), width=0.2, position=position_dodge(width=0.3)) +
  theme(panel.background = element_rect(fill="white",colour="grey"), text = element_text(size=28), legend.position = "bottom", axis.title.x=element_blank(), legend.title = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("ATE")

#ggsave("05_FieldExperiment/age_ate.png", height = 15, width = 25, units="cm")

# calibration test

test_calibration(cf)