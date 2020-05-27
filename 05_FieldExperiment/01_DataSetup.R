library(foreign)
library(sandwich)
library(lmtest)
library(grf)
library(ggplot2)
library(tidyverse)
library(xtable)

library(causalToolbox)

data <- read.dta("05_FieldExperiment/broockman_kalla_replication_data.dta")

# Define Outputs
# Main: General Trans Acceptance Attitudes
# Secondary: Gender Non-Conformity and Law Only
main.dv.names <- c('miami_trans_law_t1', 'miami_trans_law2_t1', 'therm_trans_t1', 'gender_norm_sexchange_t1', 'gender_norm_moral_t1', 'gender_norm_abnormal_t1', 'gender_norm_trans_moral_wrong_t1')
secondary.dv.names <- c('gender_norm_rights_t1', 'gender_norm_looks_t1')
legal.dv.names <- c('miami_trans_law_t1', 'miami_trans_law2_t1')

# all voters who came to their door
data <- subset(data, contacted == 1)

# all respondents to follow up surveys: 3-day, 3-week, 6-week, 3-month
data_t1 <- subset(data, respondent_t1 == 1)
data_t2 <- subset(data, respondent_t2 == 1)
data_t3 <- subset(data, respondent_t3 == 1)
data_t4 <- subset(data, respondent_t4 == 1)

# INDICES
# Pre specified index of all primary outcomes (MAIN)
all.dv.names.t1 <- c('miami_trans_law_t1', 'miami_trans_law2_t1', 'therm_trans_t1', 'gender_norm_sexchange_t1', 'gender_norm_moral_t1',
                     'gender_norm_abnormal_t1', 'gender_norm_trans_moral_wrong_t1')
all.dv.names.t2 <- c('miami_trans_law_t2', 'miami_trans_law2_t2', 'therm_trans_t2', 'gender_norm_sexchange_t2', 'gender_norm_moral_t2',
                     'gender_norm_abnormal_t2', 'gender_norm_trans_moral_wrong_t2')
all.dv.names.t3 <- c('miami_trans_law_withdef_t3', 'miami_trans_law2_withdef_t3', 'therm_trans_t3', 'gender_norm_sexchange_t3',
                     'gender_norm_moral_t3', 'gender_norm_abnormal_t3','gender_norm_trans_moral_wrong_t3')
all.dv.names.t4 <- c('miami_trans_law_withdef_t4', 'miami_trans_law2_withdef_t4', 'therm_trans_t4', 'gender_norm_sexchange_t4',
                     'gender_norm_moral_t4', 'gender_norm_abnormal_t4', 'gender_norm_trans_moral_wrong_t4')

# Transgener Tolerance Index
trans.tolerance.dvs.t0 <- c('therm_trans_t0', 'gender_norms_sexchange_t0', 'gender_norms_moral_t0', 'gender_norms_abnormal_t0')
trans.tolerance.dvs.t1 <- c('therm_trans_t1', 'gender_norm_sexchange_t1', 'gender_norm_moral_t1', 'gender_norm_abnormal_t1',
                            'gender_norm_trans_moral_wrong_t1')
trans.tolerance.dvs.t2 <- c('therm_trans_t2', 'gender_norm_sexchange_t2', 'gender_norm_moral_t2', 'gender_norm_abnormal_t2',
                            'gender_norm_trans_moral_wrong_t2', 'trans_teacher_t2', 'trans_bathroom_t2')
trans.tolerance.dvs.t3 <- c('therm_trans_t3', 'gender_norm_sexchange_t3', 'gender_norm_moral_t3', 'gender_norm_abnormal_t3',
                            'gender_norm_trans_moral_wrong_t3', 'trans_teacher_t3', 'trans_bathroom_t3')
trans.tolerance.dvs.t4 <- c('therm_trans_t4', 'gender_norm_sexchange_t4', 'gender_norm_moral_t4', 'gender_norm_abnormal_t4',
                            'gender_norm_trans_moral_wrong_t4', 'trans_teacher_t4', 'trans_bathroom_t4')

# Law items index
trans.law.dvs.t0 <- c('miami_trans_law_t0', 'miami_trans_law2_t0')
trans.law.dvs.t1 <- c('miami_trans_law_t1', 'miami_trans_law2_t1')
trans.law.dvs.t2 <- c('miami_trans_law_t2', 'miami_trans_law2_t2')
# Note: Beginning with t3, the definition was added.
trans.law.dvs.t3 <- c('miami_trans_law_withdef_t3', 'miami_trans_law2_withdef_t3')
trans.law.dvs.t4 <- c('miami_trans_law_withdef_t4', 'miami_trans_law2_withdef_t4')

# Procedure for combining multiple items into indices
# Compute factor analysis outcome
compute.factor.dv <- function(dv.names, respondent.booleans, print.loadings = TRUE){
  responders <- data[respondent.booleans,]
  # Factor analysis
  factor.obj <- princomp(responders[, dv.names], cor=TRUE)
  if(print.loadings) print(loadings(factor.obj))
  dv <- as.vector(factor.obj$scores[,1]) # Werte der ersten Komponente
  # More positive values on the factor should indicate more tolerance; reverse otherwise.
  if(cor(dv, responders$miami_trans_law_t0, use="complete.obs") < 0) dv <- -1 * dv
  # Put in the order of the main data frame
  dv.in.order <- dv[match(data$id, responders$id)]
  # Rescale to mean 0 sd 1 in placebo group; treatment effects can then be interpreted 
  # as the effect in standard deviations the treatment would have among an untreated
  # population.
  dv.in.order <- (dv.in.order - mean(dv.in.order[!data$treat_ind], na.rm=TRUE)) / sd(dv.in.order[!data$treat_ind], na.rm=TRUE)
  return(as.vector(dv.in.order))
}

# all respondents in t1
bools_t0 <- (!is.na(data$respondent_t0) & data$respondent_t0 ==1)
bools_t1 <- (!is.na(data$respondent_t1) & data$respondent_t1 ==1)
bools_t2 <- (!is.na(data$respondent_t2) & data$respondent_t2 ==1)
bools_t3 <- (!is.na(data$respondent_t3) & data$respondent_t3 ==1)
bools_t4 <- (!is.na(data$respondent_t4) & data$respondent_t4 ==1)

# fill missing values
data$vf_age[which(is.na(data$vf_age))] <- mean(data$vf_age, na.rm=TRUE)
data$survey_language_es[is.na(data$survey_language_es)] <- data$survey_language_t0[is.na(data$survey_language_es)] == "ES"

# translate strings into binary 0,1
data$exp_actual_convo <- ifelse(data$exp_actual_convo == "Trans-Equality",1,0)

# Ordinary least squares with cluster robust standard errors
# covariates
t0.covariate.names <- c('miami_trans_law_t0', 'miami_trans_law2_t0', 'therm_trans_t0', 'gender_norms_sexchange_t0',
                        'gender_norms_moral_t0', 'gender_norms_abnormal_t0', 'ssm_t0', 'therm_obama_t0', 'therm_gay_t0','vf_democrat',
                        'ideology_t0', 'religious_t0', 'exposure_gay_t0', 'exposure_trans_t0', 'pid_t0', 'sdo_scale', 'gender_norm_daugher_t0',
                        'gender_norm_looks_t0', 'gender_norm_rights_t0', 'therm_afams_t0', 'vf_female', 'vf_hispanic', 'vf_black', 'vf_age',
                        'survey_language_es', 'cluster_level_t0_scale_mean')
x <- data[,c(t0.covariate.names)]
x <- as.matrix(x, dimnames = list(NULL, names(x)))


# Function to compute clustered standard errors
cl <- function(fm, cluster){
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj <- apply(estfun(fm), 2, function(x) tapply(x, cluster, sum))
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL)
}

# Function to extract the average treatment effect from OLS with clustered SEs.
est.ate <- function(dv, include.obs = NULL, include.covariates = TRUE){
  if(is.null(include.obs)){
    include.obs <- !is.na(dv)
  }
  include.obs <- which(include.obs & !is.na(dv))
  if(include.covariates) {
    lm.obj <- lm(dv[include.obs] ~ data$treat_ind[include.obs] + x[include.obs,])
  } 
  if(!include.covariates) {
    lm.obj <- lm(dv[include.obs] ~ data$treat_ind[include.obs])
  }
  # Calculate cluster-robust standard errors.
  result <- cl(lm.obj, data$hh_id[include.obs])[2,]
  
  # Adjust point estimate and standard error for contact rate in subsample.
  #itt_d <- lm(exp_actual_convo ~ treat_ind, data[include.obs,])$coefficients[2]
  #result[1:2] <- result[1:2] / itt_d
  # Note that all DVs were recoded such that higher values indicated more tolerance.
  #result[4] <- result[4] / 2
  #p-value corresponds to mass under one side of distribution. 
  rejection.region <- ((result[4] < .04 & result[1] > 0) | (result[4] < .01 & result[1] < 0))
  # Significant positive result.
  
  # Significant negative result.
  result <- round(result, 3)
  #if(rejection.region) result[4] <- paste0(as.character(result[4]), "*")
  #if(!rejection.region) result[4] <- "*n.s.*"
  #if(result[4] == "0*") result[4] <- "0.000*"
  # Indicate precision of 0 p-value.
  #names(result)[4] <- "*p*"
  return(result[1:2])
}

# transform household ids into numeric values
# this is needed to cluster observations
data$hh_id_numeric <- as.numeric(factor(data$hh_id))

grf.est.ate <- function(dv, include.obs = NULL) {
  if(is.null(include.obs)){
    include.obs <- !is.na(dv)
  }
  grf.tau <- causal_forest(x[include.obs,], dv[include.obs], data$treat_ind[include.obs], num.trees = 4000,
                           clusters=data$hh_id_numeric[include.obs],seed=1)
  ate <- average_treatment_effect(grf.tau)
  var_imp <- variable_importance(grf.tau)
  var_imp.idx <- which(var_imp > mean(var_imp))
  return(round(ate, 3))
}

# compute outcome indices
# all
all.dv.t1 <- compute.factor.dv(all.dv.names.t1, bools_t1)
all.dv.t2 <- compute.factor.dv(all.dv.names.t2, bools_t2)
all.dv.t3 <- compute.factor.dv(all.dv.names.t3, bools_t3)
all.dv.t4 <- compute.factor.dv(all.dv.names.t4, bools_t4)

# trans.tolerance
trans.tolerance.dv.t1 <- compute.factor.dv(trans.tolerance.dvs.t1, bools_t1)
trans.tolerance.dv.t2 <- compute.factor.dv(trans.tolerance.dvs.t2, bools_t2)
trans.tolerance.dv.t3 <- compute.factor.dv(trans.tolerance.dvs.t3, bools_t3)
trans.tolerance.dv.t4 <- compute.factor.dv(trans.tolerance.dvs.t4, bools_t4)

# trans law
trans.law.dv.t1 <- compute.factor.dv(trans.law.dvs.t1, bools_t1)
trans.law.dv.t2 <- compute.factor.dv(trans.law.dvs.t2, bools_t2)
trans.law.dv.t3 <- compute.factor.dv(trans.law.dvs.t3, bools_t3)
trans.law.dv.t4 <- compute.factor.dv(trans.law.dvs.t4, bools_t4)


lin.t1 <- as.data.frame(rbind(est.ate(all.dv.t1),
                              est.ate(trans.tolerance.dv.t1),
                              est.ate(trans.law.dv.t1)),
                        row.names = c('all.dv.t1',
                                      'trans.tolerance.dv.t1',
                                      'trans.law.dv.t1'))

lin.t2 <- as.data.frame(rbind(est.ate(all.dv.t2),
                              est.ate(trans.tolerance.dv.t2),
                              est.ate(trans.law.dv.t2)),
                        row.names = c('all.dv.t2',
                                      'trans.tolerance.dv.t2',
                                      'trans.law.dv.t2'))

lin.t3 <- as.data.frame(rbind(est.ate(all.dv.t3),
                              est.ate(trans.tolerance.dv.t3),
                              est.ate(trans.law.dv.t3)),
                        row.names = c('all.dv.t3',
                                      'trans.tolerance.dv.t3',
                                      'trans.law.dv.t3'))

lin.t4 <- as.data.frame(rbind(est.ate(all.dv.t4),
                              est.ate(trans.tolerance.dv.t4),
                              est.ate(trans.law.dv.t4)),
                        row.names = c('all.dv.t4',
                                      'trans.tolerance.dv.t4',
                                      'trans.law.dv.t4'))

# GRF

grf.t1 <- as.data.frame(rbind(grf.est.ate(all.dv.t1),
                              grf.est.ate(trans.tolerance.dv.t1),
                              grf.est.ate(trans.law.dv.t1)),
                        row.names = c('all.dv.t1',
                                      'trans.tolerance.dv.t1',
                                      'trans.law.dv.t1'))

grf.t2 <- as.data.frame(rbind(grf.est.ate(all.dv.t2),
                              grf.est.ate(trans.tolerance.dv.t2),
                              grf.est.ate(trans.law.dv.t2)),
                        row.names = c('all.dv.t2',
                                      'trans.tolerance.dv.t2',
                                      'trans.law.dv.t2'))

grf.t3 <- as.data.frame(rbind(grf.est.ate(all.dv.t3),
                              grf.est.ate(trans.tolerance.dv.t3),
                              grf.est.ate(trans.law.dv.t3)),
                        row.names = c('all.dv.t3',
                                      'trans.tolerance.dv.t3',
                                      'trans.law.dv.t3'))

grf.t4 <- as.data.frame(rbind(grf.est.ate(all.dv.t4),
                              grf.est.ate(trans.tolerance.dv.t4),
                              grf.est.ate(trans.law.dv.t4)),
                        row.names = c('all.dv.t4',
                                      'trans.tolerance.dv.t4',
                                      'trans.law.dv.t4'))

# Write ATE results to table

lin <- cbind(lin.t1,lin.t2,lin.t3,lin.t4)
grf <- cbind(grf.t1,grf.t2,grf.t3,grf.t4)

colnames(grf) <- colnames(lin)

ate_results <- rbind(lin,grf, stringsAsFactors=FALSE)

print(xtable(ate_results[c("all.dv.t1",
                           "all.dv.t11",
                           "trans.tolerance.dv.t1",
                           "trans.tolerance.dv.t11",
                           "trans.law.dv.t1",
                           "trans.law.dv.t11"),], digits=3), floating = F, include.rownames = FALSE)

# Heterogeneity

grf.est.cate <- function(dv, include.obs = NULL) {
  if(is.null(include.obs)){
    include.obs <- !is.na(dv)
  }
  include.obs <- which(include.obs & !is.na(dv))
  grf.tau <- causal_forest(x[include.obs,], dv[include.obs], data$treat_ind[include.obs], num.trees = 4000,
                           clusters=data$hh_id_numeric[include.obs])
  cate.oob <- predict(grf.tau, estimate.variance=TRUE)
  var_imp <- variable_importance(grf.tau)
  test <- test_calibration(grf.tau)
  return(list(cate=cate.oob$predictions,
              varimp=var_imp,
              variance=cate.oob$variance.estimates,
              test=test))
  
}

# Using Sören Künzels X-Learner
xl.est.cate <- function(dv, include.obs = NULL) {
  if(is.null(include.obs)){
    include.obs <- !is.na(dv)
  }
  xl.tau <- X_RF(feat=x[include.obs,], tr=data$treat_ind[include.obs], yobs=dv[include.obs])
  xl.cate <- EstimateCate(xl.tau, x[include.obs,])
  return(xl.cate)
}

