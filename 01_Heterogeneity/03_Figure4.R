# Simulation on data drawn according to Design 1 in the presence of Strong Treatment Effect Heterogeneity
# Visualization of Variable Importance

library(ggplot2)
library(tidyverse)

load("01_Heterogeneity/03_VarImp_linear.RData")

cf <- t(result[c("cf.varimp.1",
                 "cf.varimp.2",
                 "cf.varimp.3",
                 "cf.varimp.4",
                 "cf.varimp.5",
                 "cf.varimp.6",
                 "cf.varimp.7",
                 "cf.varimp.8")])

df <- data.frame(var = rownames(cf),
                 cf = cf)

ggplot(df, aes(x=var, y=cf)) +
  geom_bar(stat = "identity")+
  scale_x_discrete(labels = c(1,2,3,4,5,6,7,8)) +
  theme(panel.background = element_rect(fill="white",colour="grey"), text = element_text(size=12),  legend.title = element_blank()) +
  labs(y="Variable Importance", x="Variable")

ggsave("01_Heterogeneity/03_Figure4.png",  width=18, height = 8, units = "cm")