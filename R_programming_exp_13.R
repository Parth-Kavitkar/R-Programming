# Load necessary packages
library(forecast)
library(tseries)

# Load the dataset
data("AirPassengers")
AP <- AirPassengers
plot(AP, main = "Monthly Air Passengers (1949–1960)", ylab = "Passengers (in thousands)", col = "blue")

# Summary statistics
summary(AP)

# Step 1: Decompose the time series
decomposed_AP <- decompose(AP)
plot(decomposed_AP)

# Step 2: Check for stationarity
adf.test(AP)   # Augmented Dickey-Fuller test

# Step 3: Apply differencing to make stationary
AP_diff <- diff(log(AP))
plot(AP_diff, main = "Differenced Log of Air Passengers", col = "darkgreen")

# Check again
adf.test(AP_diff)

# Step 4: Plot ACF and PACF
par(mfrow = c(1,2))
acf(AP_diff, main = "ACF of Differenced Series")
pacf(AP_diff, main = "PACF of Differenced Series")
par(mfrow = c(1,1))

# Step 5: Fit ARIMA model automatically
fit <- auto.arima(log(AP))
summary(fit)

# Step 6: Diagnostic plots
tsdisplay(residuals(fit), lag.max = 20, main = "Model Diagnostics")

# Step 7: Forecast for next 5 years (60 months)
forecast_AP <- forecast(fit, h = 60)
plot(forecast_AP, main = "Forecasted Air Passengers using ARIMA Model")

# Step 8: Back-transform predictions
predicted_values <- exp(forecast_AP$mean)
predicted_values

