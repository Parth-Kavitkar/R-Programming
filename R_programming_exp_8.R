# A. Logistic Regression (Binary Classification)
# Dataset: Titanic survival (titanic_train from titanic package)

# Install and load required packages
# install.packages(c("titanic", "tidyverse", "caTools", "pROC"))
library(titanic)
library(tidyverse)
library(caTools)
library(pROC)


# Load and prepare data
data("titanic_train", package = "titanic")
df <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, Fare) %>%
  mutate(
    Survived = factor(Survived, levels = c(0,1), labels = c("No","Yes")),
    Sex = factor(Sex),
    Pclass = factor(Pclass)
  )

# Handle missing values
df$Age[is.na(df$Age)] <- median(df$Age, na.rm = TRUE)

# Split into training/testing
set.seed(123)
split <- sample.split(df$Survived, SplitRatio = 0.7)
train <- subset(df, split == TRUE)
test  <- subset(df, split == FALSE)

# Build logistic regression model (GLM with binomial family)
logit_model <- glm(Survived ~ Pclass + Sex + Age + Fare, data = train, family = binomial)

# Model summary
summary(logit_model)

# Predict probabilities
test$prob <- predict(logit_model, newdata = test, type = "response")

# ROC curve and AUC
roc_obj <- roc(test$Survived, test$prob)
auc_val <- auc(roc_obj)
plot(roc_obj, col = "blue", main = paste("Logistic Regression ROC (AUC =", round(auc_val,3), ")"))
print(paste("AUC =", round(auc_val,3)))


# B. Poisson Regression (Count Data)
# Dataset: warpbreaks (built-in dataset in R)

# Load data
data("warpbreaks")
head(warpbreaks)

# Fit Poisson regression model
pois_model <- glm(breaks ~ wool + tension, data = warpbreaks, family = poisson)

# Model summary
summary(pois_model)

# Predicted counts
warpbreaks$pred <- predict(pois_model, type = "response")
head(warpbreaks[, c("breaks", "pred")])

# Plot observed vs predicted counts
ggplot(warpbreaks, aes(x = pred, y = breaks, color = tension)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Poisson Regression: Observed vs Predicted Counts",
       x = "Predicted Breaks", y = "Observed Breaks")

