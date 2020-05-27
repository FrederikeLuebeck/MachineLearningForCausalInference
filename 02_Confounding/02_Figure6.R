# Simulations on data drawn according to Design 2 in the presence of confounding
# Thesis: Figure 6

library(doParallel)
library(grf)
library(doSNOW)

source("00_FunctionSetup.R")

set.seed(1)

n <- 2000
n.test <- 1000
d <- 10
noise <- 0.5

data <- design2(n,
                n.test,
                d,
                noise = noise,
                form = "linear")

cf_lc <- CF_estimator(data$X,
                      data$X.test,
                      data$Y,
                      data$W)
cf_lc.evaluation <- evaluate(cf_lc$predictions, data$tau, cf_lc$sigma)

cf <- CF_estimator(data$X,
                   data$X.test,
                   data$Y,
                   data$W,
                   Y.hat = rep(0,n),
                   W.hat = rep(0,n))
cf.evaluation <- evaluate(cf$predictions, data$tau, cf$sigma)

# Visualization of Results

predictions <- data.frame(X1 = data$X.test[,1],
                          true = data$tau,
                          cf_lc = cf_lc$predictions,
                          cf = cf$predictions)
df <- predictions %>%
  select(X1, true, cf_lc, cf) %>%
  gather(key="method", value="value", -X1)

ggplot(df, aes(x=X1, y=value, color=method)) +
  geom_point(size=0.4) +
  scale_color_manual(values = c("brown1", "turquoise3", "grey69"), labels=c("CF without Local Centering", "CF with Local Centering", "True Effect")) +
  theme(panel.background = element_rect(fill="white",colour="grey"), text = element_text(size=14), legend.position = "top",
        legend.title = element_blank()) +
  labs(y="Treatment Effect", x="X1")

ggsave("02_Confounding/02_Figure6a.png", width=17, height = 11, units = "cm")

df <- data.frame(cf_lc = cf_lc$var.imp,
                 cf = cf$var.imp)

df$var <- as.numeric(rownames(df))
df <- df %>%
  gather(key="method", value="value", -var)

ggplot(df, aes(x=var, y=value, fill=method)) +
  geom_bar(stat = "identity", position = position_dodge())

ggsave("02_Confounding/02_VarImp.png", width=17, height = 11, units = "cm")

