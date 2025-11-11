# install.packages(c("MASS", "glmnet", "caret")) - IF NOT INSTALLED ALREADY
library(MASS)
library(glmnet)
library(caret)

# Step 1: Prepare Data
# Load dataset
data("Boston")

# Split features and target
x <- as.matrix(Boston[, -14])   # predictors
y <- Boston$medv                # response variable

# Standardize features (glmnet does this internally, but good practice)
x <- scale(x)

# Step 2: LASSO Regression with Cross-validation
set.seed(123)

# Perform 10-fold cross-validation
cv.lasso <- cv.glmnet(x, y, alpha = 1, nfolds = 10)

# Plot cross-validation results
plot(cv.lasso)
title("Cross-Validation for Optimal Lambda (LASSO)", line = 2.5)

# Step 3: Optimal Model Selection
# Best lambda values
best_lambda <- cv.lasso$lambda.min
best_lambda

# Step 4: Refit the Model with Best λ
lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

# Display coefficients
coef(lasso_model)

# Step 5: Model Evaluation (RMSE & R²)
# Predictions
y_pred <- predict(lasso_model, s = best_lambda, newx = x)

# RMSE
rmse <- sqrt(mean((y - y_pred)^2))
rmse

# R²
sst <- sum((y - mean(y))^2)
sse <- sum((y - y_pred)^2)
r2 <- 1 - sse / sst
r2

# Step 6: Cross-Validation Performance Summary
cat("Optimal Lambda (λ):", best_lambda, "\n")
cat("RMSE:", round(rmse, 2), "\n")
cat("R²:", round(r2, 3), "\n")

# Step 7: Compare with Simple Linear Model
lm_model <- lm(medv ~ ., data = Boston)
lm_pred <- predict(lm_model, Boston)

rmse_lm <- sqrt(mean((Boston$medv - lm_pred)^2))
r2_lm <- summary(lm_model)$r.squared

cat("Linear Model RMSE:",
    round(rmse_lm, 2),
    " | R²:",
    round(r2_lm, 3),
    "\n")
cat("LASSO Model RMSE:", round(rmse, 2), " | R²:", round(r2, 3), "\n")

# Visualization: Predicted vs Actual
plot(
  y,
  y_pred,
  main = "Actual vs Predicted Housing Prices (LASSO)",
  xlab = "Actual MEDV",
  ylab = "Predicted MEDV",
  col = "blue",
  pch = 19
)
abline(0, 1, col = "red", lwd = 2)

