# This script analyzes time series data using ARIMA, SARIMA, and Linear Regression models. 
# It includes data visualization, residual analysis, and performance metric calculations.

# Install and load required packages
required_packages <- c("forecast", "tseries", "ggplot2")
install_missing_packages <- function(packages) {
  missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(missing_packages) > 0) {
    install.packages(missing_packages)
  }
}
install_missing_packages(required_packages)
lapply(required_packages, library, character.only = TRUE)

# Read input data
input_file <- "agg_ratio_and_vwap_with_volume.csv"
time_series_data <- read.csv(input_file)

# Convert to time series format
synthetic_ts <- ts(time_series_data$VWAP, start = 1)  # Assume period is 50

# Fit ARIMA model
arima_model <- auto.arima(synthetic_ts)  # Automatically choose p, d, q

# View ARIMA model results
summary(arima_model)

# Generate ARIMA fitted values
fitted_values_arima <- fitted(arima_model)

# Fit Linear Regression model
time_index <- seq_along(synthetic_ts)
linear_model <- lm(synthetic_ts ~ time_index)

# Generate Linear Regression fitted values
fitted_values_linear <- predict(linear_model, newdata = data.frame(time_index = time_index))

# Fit SARIMA model
sarima_model <- auto.arima(synthetic_ts, seasonal = TRUE)

# View SARIMA model results
summary(sarima_model)

# Generate SARIMA fitted values
fitted_values_sarima <- fitted(sarima_model)

# Plot original data, ARIMA, Linear Regression, and SARIMA fitted values
plot(synthetic_ts, col = "blue", main = "ARIMA vs SARIMA vs Linear Regression Fitting", xlab = "Time", ylab = "Value", lwd = 2)
lines(fitted_values_arima, col = "red", lwd = 2)
lines(fitted_values_linear, col = "green", lwd = 2, lty = 2)
lines(fitted_values_sarima, col = "purple", lwd = 2, lty = 3)
legend("topleft", legend = c("Original Data", "ARIMA Fitted", "Linear Regression Fitted", "SARIMA Fitted"), 
       col = c("blue", "red", "green", "purple"), lty = c(1, 1, 2, 3), lwd = 2)

# Residual analysis
residuals_arima <- residuals(arima_model)

# ACF and PACF plots for residuals
par(mfrow = c(1, 1))
acf(residuals_arima, main = "ACF of ARIMA Residuals")
pacf(residuals_arima, main = "PACF of ARIMA Residuals")

# Shapiro-Wilk normality test
shapiro_test <- shapiro.test(residuals_arima)
cat("Shapiro-Wilk Test for ARIMA Residuals:\n")
print(shapiro_test)

# QQ plot
qqnorm(residuals_arima, main = "QQ Plot of ARIMA Residuals")
qqline(residuals_arima, col = "red", lwd = 2)

# Check residuals of SARIMA model
checkresiduals(sarima_model)

# Calculate MAPE and RMSE
calculate_metrics <- function(actual, fitted) {
  mape <- mean(abs((actual - fitted) / actual)) * 100
  rmse <- sqrt(mean((actual - fitted)^2))
  return(list(MAPE = mape, RMSE = rmse))
}

# Actual data (excluding NA values)
actual_values <- na.omit(synthetic_ts)

# Calculate MAPE and RMSE for ARIMA
metrics_arima <- calculate_metrics(actual_values, fitted_values_arima)
cat("ARIMA Metrics:\n")
print(metrics_arima)

# Calculate MAPE and RMSE for Linear Regression
metrics_linear <- calculate_metrics(actual_values, fitted_values_linear)
cat("Linear Regression Metrics:\n")
print(metrics_linear)

# Calculate MAPE and RMSE for SARIMA
metrics_sarima <- calculate_metrics(actual_values, fitted_values_sarima)
cat("SARIMA Metrics:\n")
print(metrics_sarima)
