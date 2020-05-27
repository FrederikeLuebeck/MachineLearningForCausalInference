# Exploring Heterogeneity on all Indices

library(foreign)
library(sandwich)
library(lmtest)
library(grf)
library(ggplot2)
library(tidyverse)

library(causalToolbox)

source("05_FieldExperiment/01_DataSetup.R")

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

# Histograms of all CATE Estimates

df_t1 <- data.frame(x=grf.cate.all.t1$cate, t=1, m="all", y=grf.t1["all.dv.t1", "estimate"])
df_t2 <- data.frame(x=grf.cate.all.t2$cate, t=2, m="all", y=grf.t2["all.dv.t2", "estimate"])
df_t3 <- data.frame(x=grf.cate.all.t3$cate, t=3, m="all", y=grf.t3["all.dv.t3", "estimate"])
df_t4 <- data.frame(x=grf.cate.all.t4$cate, t=4, m="all", y=grf.t4["all.dv.t4", "estimate"])
ff_t1 <- data.frame(x=grf.cate.trans.t1$cate, t=1, m="trans", y=grf.t1["trans.tolerance.dv.t1", "estimate"])
ff_t2 <- data.frame(x=grf.cate.trans.t2$cate, t=2, m="trans", y=grf.t2["trans.tolerance.dv.t2", "estimate"])
ff_t3 <- data.frame(x=grf.cate.trans.t3$cate, t=3, m="trans", y=grf.t3["trans.tolerance.dv.t3", "estimate"])
ff_t4 <- data.frame(x=grf.cate.trans.t4$cate, t=4, m="trans", y=grf.t4["trans.tolerance.dv.t4", "estimate"])
f_t1 <- data.frame(x=grf.cate.law.t1$cate, t=1, m="law", y=grf.t1["trans.law.dv.t1", "estimate"])
f_t2 <- data.frame(x=grf.cate.law.t2$cate, t=2, m="law", y=grf.t2["trans.law.dv.t2", "estimate"])
f_t3 <- data.frame(x=grf.cate.law.t3$cate, t=3, m="law", y=grf.t3["trans.law.dv.t3", "estimate"])
f_t4 <- data.frame(x=grf.cate.law.t4$cate, t=4, m="law", y=grf.t4["trans.law.dv.t4", "estimate"])

cates <- rbind(df_t1,df_t2,df_t3,df_t4,ff_t1,ff_t2,ff_t3,ff_t4,f_t1,f_t2,f_t3,f_t4)
cates$m <- factor(cates$m, labels=c("Index 1: All", "Index 2: Transgender Prejudice","Index 3: Law"))


ggplot(cates, aes(x=x)) +
  geom_histogram(color="white", fill="turquoise3", bins=40) +
  geom_vline(aes(xintercept=y), color="brown1", size=1.2) +
  theme(panel.background = element_rect(fill="white",colour="grey"), text = element_text(size=20)) +
  labs(x="Treatment Effect Estimates") +
  facet_grid(vars(t), vars(m))

#ggsave("05_FieldExperiment/Histograms.png")

output <- matrix(ncol=5,nrow=12)
i=1
for (o in list(grf.cate.all.t1,
               grf.cate.all.t2,
               grf.cate.all.t3,
               grf.cate.all.t4,
               grf.cate.trans.t1,
               grf.cate.trans.t2,
               grf.cate.trans.t3,
               grf.cate.trans.t4,
               grf.cate.law.t1,
               grf.cate.law.t2,
               grf.cate.law.t3,
               grf.cate.law.t4)) {
  output[i,] = c(mean(o$cate),
                 sd(o$cate),
                 min(o$cate),
                 max(o$cate),
                 mean(sqrt(o$variance)))
  i = i+1
}

print(xtable(output, digits=3), floating = F, include.rownames = FALSE)

# calibration test
a <- grf.cate.all.t1$test

output <- matrix(ncol=2,nrow=12)
i=1
for (o in list(grf.cate.all.t1,
               grf.cate.all.t2,
               grf.cate.all.t3,
               grf.cate.all.t4,
               grf.cate.trans.t1,
               grf.cate.trans.t2,
               grf.cate.trans.t3,
               grf.cate.trans.t4,
               grf.cate.law.t1,
               grf.cate.law.t2,
               grf.cate.law.t3,
               grf.cate.law.t4)) {
  output[i,] = c(o$test[1],
                 o$test[2])
  i = i+1
}

print(xtable(output, digits=3), floating = F, include.rownames = FALSE)
