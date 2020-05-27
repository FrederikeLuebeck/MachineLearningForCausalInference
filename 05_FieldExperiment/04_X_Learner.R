# Comparison to X-Learner

library(foreign)
library(sandwich)
library(lmtest)
library(grf)
library(ggplot2)
library(tidyverse)
library(causalToolbox)

source("05_FieldExperiment/01_DataSetup.R")

# estimate all cates
xl.cate.trans.t1 <- xl.est.cate(trans.tolerance.dv.t1)
xl.cate.trans.t2 <- xl.est.cate(trans.tolerance.dv.t2)
xl.cate.trans.t3 <- xl.est.cate(trans.tolerance.dv.t3)
xl.cate.trans.t4 <- xl.est.cate(trans.tolerance.dv.t4)

xl.cate.all.t1 <- xl.est.cate(all.dv.t1)
xl.cate.all.t2 <- xl.est.cate(all.dv.t2)
xl.cate.all.t3 <- xl.est.cate(all.dv.t3)
xl.cate.all.t4 <- xl.est.cate(all.dv.t4)

xl.cate.law.t1 <- xl.est.cate(trans.law.dv.t1)
xl.cate.law.t2 <- xl.est.cate(trans.law.dv.t2)
xl.cate.law.t3 <- xl.est.cate(trans.law.dv.t3)
xl.cate.law.t4 <- xl.est.cate(trans.law.dv.t4)

# dataframes for plotting 

df_t1 <- data.frame(x=xl.cate.all.t1, t=1, m="all")
df_t2 <- data.frame(x=xl.cate.all.t2, t=2, m="all")
df_t3 <- data.frame(x=xl.cate.all.t3, t=3, m="all")
df_t4 <- data.frame(x=xl.cate.all.t4, t=4, m="all")
ff_t1 <- data.frame(x=xl.cate.trans.t1, t=1, m="trans")
ff_t2 <- data.frame(x=xl.cate.trans.t2, t=2, m="trans")
ff_t3 <- data.frame(x=xl.cate.trans.t3, t=3, m="trans")
ff_t4 <- data.frame(x=xl.cate.trans.t4, t=4, m="trans")
f_t1 <- data.frame(x=xl.cate.law.t1, t=1, m="law")
f_t2 <- data.frame(x=xl.cate.law.t2, t=2, m="law")
f_t3 <- data.frame(x=xl.cate.law.t3, t=3, m="law")
f_t4 <- data.frame(x=xl.cate.law.t4, t=4, m="law")


cates <- rbind(df_t1,df_t2,df_t3,df_t4,ff_t1,ff_t2,ff_t3,ff_t4,f_t1,f_t2,f_t3,f_t4)
cates$m <- factor(cates$m, labels=c("Index 1: All", "Index 2: Transgender Prejudice","Index 3: Law"))
cates$e <- "X-Learner"

grf.cate.trans.t1 <- grf.est.cate(trans.tolerance.dv.t1)
grf.cate.trans.t2 <- grf.est.cate(trans.tolerance.dv.t2)
grf.cate.trans.t3 <- grf.est.cate(trans.tolerance.dv.t3)
grf.cate.trans.t4 <- grf.est.cate(trans.tolerance.dv.t4)

grf.cate.all.t1 <- grf.est.cate(all.dv.t1)
grf.cate.all.t2 <- grf.est.cate(all.dv.t2)
grf.cate.all.t3 <- grf.est.cate(all.dv.t3)
grf.cate.all.t4 <- grf.est.cate(all.dv.t4)

grf.cate.law.t1 <- grf.est.cate(trans.law.dv.t1)
grf.cate.law.t2 <- grf.est.cate(trans.law.dv.t2)
grf.cate.law.t3 <- grf.est.cate(trans.law.dv.t3)
grf.cate.law.t4 <- grf.est.cate(trans.law.dv.t4)

df_t1 <- data.frame(x=grf.cate.all.t1$cate, t=1, m="all")
df_t2 <- data.frame(x=grf.cate.all.t2$cate, t=2, m="all")
df_t3 <- data.frame(x=grf.cate.all.t3$cate, t=3, m="all")
df_t4 <- data.frame(x=grf.cate.all.t4$cate, t=4, m="all")
ff_t1 <- data.frame(x=grf.cate.trans.t1$cate, t=1, m="trans")
ff_t2 <- data.frame(x=grf.cate.trans.t2$cate, t=2, m="trans")
ff_t3 <- data.frame(x=grf.cate.trans.t3$cate, t=3, m="trans")
ff_t4 <- data.frame(x=grf.cate.trans.t4$cate, t=4, m="trans")
f_t1 <- data.frame(x=grf.cate.law.t1$cate, t=1, m="law")
f_t2 <- data.frame(x=grf.cate.law.t2$cate, t=2, m="law")
f_t3 <- data.frame(x=grf.cate.law.t3$cate, t=3, m="law")
f_t4 <- data.frame(x=grf.cate.law.t4$cate, t=4, m="law")

cates2 <- rbind(df_t1,df_t2,df_t3,df_t4,ff_t1,ff_t2,ff_t3,ff_t4,f_t1,f_t2,f_t3,f_t4)
cates2$m <- factor(cates$m, labels=c("Index 1: All", "Index 2: Transgender Prejudice","Index 3: Law"))
cates2$e <- "Causal Forest"

cates_both <- rbind(cates2, cates)

ggplot(cates_both, aes(x=x, fill=e)) +
  geom_histogram(alpha=0.5, position="identity", bins=40) +
  theme(panel.background = element_rect(fill="white",colour="grey"), text = element_text(size=20), legend.position = "top", legend.title = element_blank()) +
  labs(x="Treatment Effect Estimates") +
  facet_grid(vars(t), vars(m))

#ggsave("05_FieldExperiment/XL_Histograms.png")