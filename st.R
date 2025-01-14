# This script performs time series analysis using ARIMA and SARIMA models. It includes data transformation, 
# autocorrelation tests, model diagnostics, forecasting, residual analysis, and visualization.

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

# Read saved time series data
input_file <- "agg_ratio_and_vwap_with_volume.csv"
time_series_data <- read.csv(input_file)

# Assume the time series data is in the "VWAP" column
time_series <- ts(time_series_data$VWAP)

start_time <- 10
period <- 40
time_series <- time_series  # Use the full time series

# Log transformation to stabilize variance
log_series <- log(time_series)

# Differencing to remove trend
log_diff_series <- diff(log_series)

# Data split: 80% training set, 20% test set
train_size <- floor(0.9 * length(log_diff_series))
train_series <- ts(log_diff_series[1:train_size])
test_series <- ts(log_diff_series[(train_size + 1):length(log_diff_series)])

# 1. Decorrelation Tests

# (a) Ljung-Box test
ljung_box_result <- Box.test(train_series, lag = 10, type = "Ljung-Box")
cat("Ljung-Box test results:\n")
print(ljung_box_result)

# (b) Box-Pierce test
box_pierce_result <- Box.test(train_series, lag = 10, type = "Box-Pierce")
cat("\nBox-Pierce test results:\n")
print(box_pierce_result)

# (c) Bartlett test (confidence interval for sample autocorrelations)
acf_values <- acf(train_series, plot = FALSE)$acf
bartlett_result <- qnorm(0.975) / sqrt(length(train_series))
cat("\nBartlett test results:\n")
cat("Confidence interval is +/-", bartlett_result, "\n")

# 2. Plot differenced time series
plot(train_series, main = "Differenced Log Time Series", ylab = "Differenced Values", xlab = "Time")

# (b) Augmented Dickey-Fuller test (unit root test)
adf_result <- adf.test(train_series)
cat("\nADF unit root test results:\n")
print(adf_result)

# 3. ARIMA Model Introduction

# (a) Automatic ARIMA fitting
arima_fit <- auto.arima(train_series)
cat("\nARIMA model fitting results:\n")
print(arima_fit)

# (b) Model diagnostics
checkresiduals(arima_fit)

# (c) Forecasting
forecast_result <- forecast(arima_fit, h = length(test_series))
cat("\nARIMA model forecast results:\n")
print(forecast_result)

# Plot forecast results
plot(forecast_result, main = "ARIMA Model Forecast")

# Residual analysis
cat("\nResidual analysis:\n")

# Extract residuals
residuals_arima <- residuals(arima_fit)

# Plot residual time series
plot(residuals_arima, main = "ARIMA Model Residuals", ylab = "Residuals", xlab = "Time")

# ACF and PACF plots
acf(residuals_arima, main = "ACF of Residuals")
pacf(residuals_arima, main = "PACF of Residuals")

# Ljung-Box test for residuals
ljung_box_residual <- Box.test(residuals_arima, lag = 10, type = "Ljung-Box")
cat("Ljung-Box test for residuals:\n")
print(ljung_box_residual)

# Normality test
shapiro_test <- shapiro.test(residuals_arima)
cat("\nShapiro-Wilk normality test results:\n")
print(shapiro_test)

# Check normality of residuals
qqnorm(residuals_arima)
qqline(residuals_arima, col = "red", lwd = 2)

# Histogram of residuals
hist(residuals_arima, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals", col = "skyblue")

# If residuals are not white noise or not normal, model improvement is needed
if (ljung_box_residual$p.value < 0.05 || shapiro_test$p.value < 0.05) {
  cat("Residuals are not white noise or not normally distributed. Consider improving the model.\n")
}

# 4. Seasonal Model Extension (SARIMA)
sarima_fit <- auto.arima(train_series, seasonal = TRUE, stepwise = TRUE, approximation = FALSE)
cat("\nSARIMA model fitting results:\n")
print(sarima_fit)

checkresiduals(sarima_fit)

# Model comparison
cat("\nModel comparison:\n")
aic_arima <- arima_fit$aic
aic_sarima <- sarima_fit$aic

cat("ARIMA AIC:", aic_arima, "\n")
cat("SARIMA AIC:", aic_sarima, "\n")

if (aic_sarima < aic_arima) {
  cat("SARIMA model performs better. Recommended.\n")
} else {
  cat("ARIMA model performs better. Recommended.\n")
}

# Confidence interval for the original series
original_series <- ts(log_diff_series)
quantiles <- quantile(original_series, probs = c(0.25, 0.75))
lower_bound <- quantiles[1]
upper_bound <- quantiles[2]

# Create data frame for plotting
plot_data <- data.frame(
  Time = time(original_series),
  Original = as.numeric(original_series),
  Predicted = c(rep(NA, train_size), as.numeric(forecast_result$mean)),
  Lower = c(rep(NA, train_size), as.numeric(forecast_result$lower[, 2])),
  Upper = c(rep(NA, train_size), as.numeric(forecast_result$upper[, 2]))
)

# Plot comparison
ggplot(plot_data, aes(x = Time)) +
  geom_line(aes(y = Original, color = "Original"), size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1, linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +
  geom_hline(yintercept = lower_bound, linetype = "dotted", color = "red", size = 1) +
  geom_hline(yintercept = upper_bound, linetype = "dotted", color = "red", size = 1) +
  labs(
    title = "Comparison of Original and Forecasted Series",
    x = "Time",
    y = "Values",
    color = "Series Type"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Original" = "black", "Predicted" = "blue")) +
  theme(legend.position = "top")
