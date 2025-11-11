install.packages("rpart.plot")
install.packages("randomForest")
# Load libraries
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)

# Load dataset
data(iris)
set.seed(123)

# Split data
index <- createDataPartition(iris$Species, p=0.7, list=FALSE)
train_data <- iris[index, ]
test_data  <- iris[-index, ]
# Decision Tree Model
dt_model <- rpart(Species ~ ., data=train_data, method='class')
rpart.plot(dt_model, type=2, extra=104, fallen.leaves=TRUE, main="Decision Tree")

# Prediction and Evaluation
dt_pred <- predict(dt_model, test_data, type='class')
confusionMatrix(dt_pred, test_data$Species)

# Random Forest Model
rf_model <- randomForest(Species ~ ., data=train_data, ntree=100, mtry=2, importance=TRUE)
rf_pred <- predict(rf_model, test_data)
confusionMatrix(rf_pred, test_data$Species)

# Variable Importance Plot
varImpPlot(rf_model)

# Tuning Random Forest
tuned_rf <- tuneRF(train_data[, -5], train_data[, 5], stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE)

