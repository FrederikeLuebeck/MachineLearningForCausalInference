# Visualization of Results of Simulation Design 1

library(ggplot2)
library(tidyverse)

# Plot of MSE

# Linear
load("01_Heterogeneity/01_Simulation_Design1_linear.RData")

lin <- result %>%
  select(d, cf.mse, knn8.mse, knn15.mse, knn30.mse) %>%
  gather(key="method", value="value", -d)
lin$form = "linear"

# Nonlinear
load("01_Heterogeneity/01_Simulation_Design1_nonlinear.RData")

nonlin <- result %>%
  select(d, cf.mse, knn8.mse, knn15.mse, knn30.mse) %>%
  gather(key="method", value="value", -d)
nonlin$form = "non-linear"

both <- rbind(lin,nonlin)

cols <- c("cf.mse" = "brown1",
          "knn8.mse" = "steelblue4",
          "knn15.mse" = "turquoise3",
          "knn30.mse" = "goldenrod1")

ggplot(both, aes(x=d, y=value, color=method)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = cols,
                     breaks = c("cf.mse", "knn8.mse", "knn15.mse", "knn30.mse"),
                     labels=c("Causal Forest", "8-NN", "15-NN", "30-NN")) +
  theme(panel.background = element_rect(fill="white",colour="grey"), text = element_text(size=12),  legend.title = element_blank(),
        legend.position = "top") +
  labs(y="MSE", x="d") +
  facet_wrap(~ form)

ggsave("01_Heterogeneity/01_Figure_3.png", width=18, height = 8, units = "cm")
