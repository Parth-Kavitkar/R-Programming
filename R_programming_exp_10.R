# Experiment: LASSO Regression with Cross-validation

# Load required packages
library(MASS)       # For Boston dataset
library(glmnet)     # For LASSO regression
library(caret)      # For data splitting and evaluation

# Load dataset
data("Boston")
head(Boston)

# Separate features (X) and target variable (y)
x <- as.matrix(Boston[, -14])  # All predictors
y <- Boston$medv               # Target variable (Median home value)

# Split into train and test sets (80-20)
set.seed(123)
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
x_train <- x[trainIndex, ]
y_train <- y[trainIndex]
x_test  <- x[-trainIndex, ]
y_test  <- y[-trainIndex]




# LASSO Regression Model with Cross-Validation

# Perform 10-fold cross-validation to find optimal lambda
set.seed(123)
cv.lasso <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 10)

# Plot CV curve
plot(cv.lasso)
title("LASSO Cross-Validation Curve", line = 2.5)

# Optimal lambda values
lambda_min <- cv.lasso$lambda.min
lambda_1se <- cv.lasso$lambda.1se
cat("Optimal lambda (min):", lambda_min, "\n")
cat("Optimal lambda (1se):", lambda_1se, "\n")

# Fit final LASSO model using optimal lambda
lasso.model <- glmnet(x_train, y_train, alpha = 1, lambda = lambda_min)

# Display coefficients (some may be zero)
coef(lasso.model)

# Predict on test data
y_pred <- predict(lasso.model, s = lambda_min, newx = x_test)


# Model Evaluation

# Compute performance metrics
rmse <- sqrt(mean((y_test - y_pred)^2))
r2 <- 1 - sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2)

cat("RMSE:", round(rmse, 3), "\n")
cat("R²:", round(r2, 3), "\n")

# Compare with Standard Linear Regression

lm.model <- lm(medv ~ ., data = Boston[trainIndex, ])
lm.pred <- predict(lm.model, newdata = Boston[-trainIndex, ])

lm.rmse <- sqrt(mean((y_test - lm.pred)^2))
lm.r2 <- 1 - sum((y_test - lm.pred)^2) / sum((y_test - mean(y_test))^2)

cat("\n--- Model Comparison ---\n")
cat("Linear Regression RMSE:", round(lm.rmse, 3), " | R²:", round(lm.r2, 3), "\n")
cat("LASSO Regression RMSE :", round(rmse, 3), " | R²:", round(r2, 3), "\n")

