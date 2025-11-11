# Linear Regression Analysis with mtcars dataset
# 1. Load dataset and explore
data(mtcars)
head(mtcars)
summary(mtcars)
str(mtcars)

# 2. Simple Linear Regression: mpg ~ hp
model1 <- lm(mpg ~ hp, data = mtcars)
summary(model1)

# Plot regression line
plot(mtcars$hp, mtcars$mpg, main = "MPG vs Horsepower",
     xlab = "Horsepower", ylab = "Miles Per Gallon", pch = 19, col = "darkgray")
abline(model1, col = "blue", lwd = 2)

# 3. Multiple Linear Regression: mpg ~ hp + wt + cyl
model2 <- lm(mpg ~ hp + wt + cyl, data = mtcars)
summary(model2)

# 4. Model Diagnostics
par(mfrow = c(2, 2))
plot(model2)
par(mfrow = c(1, 1))

# 5. Check Multicollinearity
if(!require(car)) install.packages('car', dependencies=TRUE)
library(car)
vif(model2)

# 6. Residual Normality
shapiro.test(residuals(model2))
                                   
# 7. Homoscedasticity (Breusch-Pagan Test)
if(!require(lmtest)) install.packages('lmtest', dependencies=TRUE)
library(lmtest)
bptest(model2)
                                                                         
# 8. Influential Observations
influence.measures(model2)
                                                                         
# 9. Compare Models
anova(model1, model2)
                                                                         
# 10. Predict on new data
newdata <- data.frame(hp = c(100, 150), wt = c(2.5, 3.0), cyl = c(4, 6))
predict(model2, newdata)

