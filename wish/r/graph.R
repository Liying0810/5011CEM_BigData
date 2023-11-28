# Actual units sold from the test data
units_sold.act <- test_df$units_sold

# Predicted units sold using each of the four models
units_sold.pred <- predict(fit.lm, test_df)
units_sold.pred2 <- predict(fit.lm2, test_df)
units_sold.pred3 <- predict(fit.lm3, test_df)
units_sold.pred4 <- predict(fit.ireg, test_df)

# Calculate errors (residuals) for each model
units_sold.error <- residuals(fit.lm)
units_sold.error2 <- residuals(fit.lm2)
units_sold.error3 <- residuals(fit.lm3)
units_sold.error4 <- residuals(fit.ireg)

# Load ggplot2 library for creating plots
library("ggplot2")

# Create scatter plots for each model
p1 <- ggplot(data = test_df, aes(x = units_sold.act, y = units_sold.pred)) +
  geom_point(pch = 19, color = "aquamarine3", size = 2) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), color = "bisque2", linetype = 2) +
  labs(title = "Simple Linear Regression", x = "Actual", y = "Predict") +
  theme_dark()

p2 <- ggplot(data = test_df, aes(x = units_sold.act, y = units_sold.pred2)) +
  geom_point(pch = 19, color = "aquamarine3", size = 2) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), color = "bisque2", linetype = 2) +
  labs(title = "Polynomial Linear Regression", x = "Actual", y = "Predict") +
  theme_dark()

p3 <- ggplot(data = test_df, aes(x = units_sold.act, y = units_sold.pred3)) +
  geom_point(pch = 19, color = "aquamarine3", size = 2) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), color = "bisque2", linetype = 2) +
  labs(title = "Multiple Linear Regression", x = "Actual", y = "Predict") +
  theme_dark()

p4 <- ggplot(data = test_df, aes(x = units_sold.act, y = units_sold.pred4)) +
  geom_point(pch = 19, color = "aquamarine3", size = 2) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), color = "bisque2", linetype = 2) +
  labs(title = "Multiple Linear with Interaction", x = "Actual", y = "Predict") +
  theme_dark()

# Combine plots into a grid arrangement
library(gridExtra)
grid.arrange(p1,p2,p3,p4, ncol = 2)
