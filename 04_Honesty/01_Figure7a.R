# Visualization of results of simulation on data drawn according to design 4

library(ggplot2)
library(tidyverse)

load("04_Honesty/01_Noise_nonlinear.RData")

df <- result %>%
  select(noise, honest.mse, adaptive.mse) %>%
  gather(key="method", value="value", -noise)

ggplot(df, aes(x=noise, y=value, color=method)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("brown1", "turquoise3"), labels=c("Adaptive", "Honest")) +
  theme(panel.background = element_rect(fill="white",colour="grey"), text = element_text(size=18),  legend.title = element_blank(), legend.position = "top") +
  labs(y="MSE", x="Noise")

ggsave("04_Honesty/01_Figure7.png", width=17, height = 11, units = "cm")
