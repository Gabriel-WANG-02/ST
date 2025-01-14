# This script performs time series analysis using ARIMA and Linear Regression models, including data splitting into training and validation sets, model fitting, forecasting, and performance evaluation.

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

# Load data
input_file <- "agg_ratio_and_vwap_with_volume.csv"
time_series_data <- read.csv(input_file)

# Convert to time series format
synthetic_ts <- ts(time_series_data$VWAP, start = 1)  # Assume period is 50

# Convert to data frame
synthetic_data <- synthetic_ts

# Split into training and validation sets
n <- length(synthetic_data)
train_size <- floor(0.7 * n)
validation_size <- n - train_size

train_start <- 1
train_data <- synthetic_data[train_start:train_size]
validation_data <- synthetic_data[(train_size + 1):n]

# Convert to time series format
train_ts <- ts(train_data, start = 1)
validation_ts <- ts(validation_data, start = train_size)

# Fit ARIMA model
arima_model <- auto.arima(train_ts)
summary(arima_model)

# Ensure ARIMA model only accesses training set
fitted_values_arima <- fitted(arima_model)

# Generate forecasts for validation set
arima_forecast <- forecast(arima_model, h = validation_size)
validation_predictions <- arima_forecast$mean

# Fit Linear Regression model
time_index_train <- seq_along(train_ts)
linear_model <- lm(train_ts ~ time_index_train)

# Generate predictions for validation set using Linear Regression
time_index_validation <- seq(train_size + 1, n)
linear_forecast <- predict(linear_model, newdata = data.frame(time_index_train = time_index_validation))

# Plot original data, training set fit, and validation set predictions
plot(synthetic_data, col = "blue", main = "Training and Validation: ARIMA vs Linear Regression", xlab = "Time", ylab = "Value", lwd = 2)

# Highlight training set
rect(xleft = train_start, xright = train_size, ybottom = min(synthetic_data), 
     ytop = max(synthetic_data), col = rgb(0.9, 0.9, 0.9, 0.5), border = NA)

# Highlight validation set
rect(xleft = train_size + 1, xright = n, ybottom = min(synthetic_data), 
     ytop = max(synthetic_data), col = rgb(0.8, 0.9, 1, 0.5), border = NA)

# Plot ARIMA fitted values for training set
lines(seq(train_start, train_size), fitted_values_arima, col = "red", lwd = 2)

# Plot Linear Regression fitted values
lines(seq(train_start, train_size), linear_model$fitted.values, col = "green", lwd = 2, lty = 2)

# Plot validation set predictions
lines(seq(train_size + 1, n), validation_predictions, col = "red", lwd = 2)
lines(seq(train_size + 1, n), linear_forecast, col = "green", lwd = 2, lty = 2)

# Plot prediction intervals for ARIMA
lines(seq(train_size + 1, n), arima_forecast$lower[, 2], col = "red", lwd = 1, lty = 2)
lines(seq(train_size + 1, n), arima_forecast$upper[, 2], col = "red", lwd = 1, lty = 2)
polygon(c(seq(train_size + 1, n), rev(seq(train_size + 1, n))), 
        c(arima_forecast$upper[, 2], rev(arima_forecast$lower[, 2])), 
        col = rgb(1, 0, 0, 0.2), border = NA)

# Add legend
legend("bottomleft", legend = c("Original Data", "ARIMA Fitted", "Linear Regression Fitted", "ARIMA Prediction", "ARIMA Quantile Range"), 
       col = c("blue", "red", "green", "red", "red"), lty = c(1, 1, 2, 1, 1), lwd = c(2, 2, 2, 2, 1), fill = c(NA, NA, NA, NA, rgb(1, 0, 0, 0.2)), cex = 0.8)

# Calculate MAPE and RMSE for validation set
calculate_metrics <- function(actual, predicted) {
  mape <- mean(abs((actual - predicted) / actual)) * 100
  rmse <- sqrt(mean((actual - predicted)^2))
  return(list(MAPE = mape, RMSE = rmse))
}

# Compute performance metrics for each model
metrics_arima <- calculate_metrics(validation_data, validation_predictions)
cat("ARIMA Validation Metrics:\n")
print(metrics_arima)

metrics_linear <- calculate_metrics(validation_data, linear_forecast)
cat("Linear Regression Validation Metrics:\n")
print(metrics_linear)
