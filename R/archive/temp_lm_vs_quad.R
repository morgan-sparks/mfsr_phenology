# Compare linear and quadratic models for temp covariate
library(ggplot2)

plot(model_data$temp_90, model_data$yday)

# fit linear and quadratic model and compare with AIC
lm1 <- lm(yday ~ temp_90, data = model_data)
lm2 <- lm(yday ~ temp_90 + I(temp_90^2), data = model_data)
AIC(lm1, lm2)

sjPlot::tab_model(lm1, lm2)

# plot with ggplot
ggplot(model_data, aes(x = temp_90, y = yday)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red") +
  labs(title = "Linear vs Quadratic Model Fit",
       x = "Temperature (90 day)",
       y = "Day of Year")