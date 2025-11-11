library(ggplot2)

# 1. Two - sample t-test (mtcars)
data("mtcars")

t.test(mpg ~ am, data = mtcars)

boxplot(mpg ~ am, data = mtcars, main ="MPG by Transmission Type", xlab = "Transmission (0 = Auto, 1 = Mannual)",ylab = "Miles per Gallon", col = c("lightblue","lightgreen"))

anova_model <- aov(mpg ~ factor(gear), data = mtcars)
summary(anova_model)

boxplot(mpg ~ factor(gear), data = mtcars, main="MPG by Number of Gears", xlab = "Number of Gears", ylab = "Miles per Gallon", col = c("orange", "lightblue","lightgreen"))

wilcox.test(mpg ~ am, data = mtcars)

ggplot(mtcars, aes(x = factor(am), y = mpg, fill = factor(am))) + geom_violin(trim = FALSE, alpha = 0.6) + geom_boxplot(width = 0.1, outlier.color = "red")
  labs(title = "MPG Distribution by Transmission Type",x = "Transmission (0 = Auto, 1 = Manual)", y = "Miles per Gallon") + theme_minimal() 

data(airquality)
aq <- na.omit(airquality)

t.test(aq$Ozone, mu =50)

hist(aq$Ozone, breaks = 20, col = "lightblue", main = "Histogram of ozone levels", xlab = "Ozone (ppb)")
abline(v = mean(aq$Ozone), col = "red", lwd = 2)
abline(v = 50, col = "darkgreen", lwd = 2, lty = 2)
legend("topright", legend = c("Mean ozone","Hypothesized mean = 50"), col = c("red","darkgreen"),lwd = 2, lty = c(1,2))

# 5. ANOVA  (airquality)
anova_wind <- aov(Wind ~ factor(Month), data = aq)
summary(anova_wind)

boxplot(Wind ~ factor(Month), data = aq, main = "Wind Speed Across Months", xlab = "Month", ylab = "Wind Speed (mph)", col = rainbow(5))

july_solar <- subset(aq, Month == 7)$solar.R 
aug_solar <- subset(aq, Month == 8)$solar.R

wilcox.test(july_solar, aug_solar)

solar_data <- subset(aq, Month %in% c(7,8))
ggplot(solar_data, aes(x = factor(Month), y = Solar.R, fill = factor(Month))) + geom_boxplot(alpha = 0.7) + labs(title = "Solar Radiation in July vs August", x = "Month", y = "Solar Radiation") + scale_x_discrete(labels = c("7" = "July","8"="August"))+ theme_minimal()

