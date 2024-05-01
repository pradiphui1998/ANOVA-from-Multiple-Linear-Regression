## Creating  the data
X1=c(55.6,44,58.3,51.5,55.6,47.5,61.7,55.7,56.5,56.5,52.5,49.6,55.4,55.2,48.3,52.8)
X2=c(36.6,42.8,28.1,55.7,53.2,47.6,51.6,29.3,64.2,51.1,33.4,44,30.9,56.2,33.1,18.7)
Y=c(61.8,41,39.7,55.6,64.8,24.5,56.6,31.9,66.3,67.9,45.6,43.4,26.6,61.8,51.2,19)

## Creating a data frame
data=data.frame(X1,X2,Y)
data

# Step 1: Fit the linear regression model
model <- lm(Y ~ X1 + X2, data=data)

# Step 2: Overall significance of the regression model
anova_result <- anova(model)
anova_result
overall_p_value <- anova_result$`Pr(>F)`[1]

# Step 3: Sum of squares of regression
SSR <- sum((predict(model) - mean(data$Y))^2)

# Step 4: Sum of squares of residuals
SSE <- sum(model$residuals^2)

# Step 5: Calculate F-value
n <- nrow(data)
p <- length(coef(model)) - 1
DF_regression <- p
DF_residual <- n - p - 1
F_value <- (SSR / DF_regression) / (SSE / DF_residual)

# Step 6: Calculate R-squared value
R_squared <- summary(model)$r.squared



# F-statistic value
F_value <- (SSR / DF_regression) / (SSE / DF_residual)

# Calculate p-value
p_value <- pf(F_value, DF_regression, DF_residual, lower.tail = FALSE)

# Print the p-value
cat("P-value based on the F-statistic value:", p_value, "\n")


# Print the results
cat("Overall significance of the regression model (p-value):", overall_p_value, "\n")
cat("Sum of squares of regression (SSR):", SSR, "\n")
cat("Sum of squares of residuals (SSE):", SSE, "\n")
cat("F-value:", F_value, "\n")
cat("R-squared value:", R_squared, "\n")
