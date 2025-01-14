# This script evaluates the performance of a SARIMA model on time series data by analyzing the relationship 
# between training set length and prediction errors (RMSE, MAPE, Bias, Variance). It includes visualization 
# of error trends to explore bias-variance tradeoff.

# Install and load required packages
required_packages <- c("forecast", "ggplot2")
install_missing_packages <- function(packages) {
  missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(missing_packages) > 0) {
    install.packages(missing_packages)
  }
}
install_missing_packages(required_packages)
lapply(required_packages, library, character.only = TRUE)

# Read time series data
input_file <- "agg_ratio_and_vwap_with_volume.csv"
time_series_data <- read.csv(input_file)
vwap_ts <- ts(time_series_data$VWAP)

# Log transformation of VWAP
log_vwap_ts <- log(vwap_ts)

# Parameter settings
m <- 2  # Fixed number of points to forecast
max_n <- length(log_vwap_ts) - m  # Maximum training set length
n_values <- seq(5, min(1005, max_n), by = 5)  # Dynamically adjust training set lengths

# Initialize lists to store error results
rmse_values <- numeric()
mape_values <- numeric()
bias_values <- numeric()
variance_values <- numeric()

# Iterate over different training set lengths
for (n in n_values) {
  predicted <- c()
  actual <- c()
  
  # Sliding window
  start_indices <- seq(1, length(log_vwap_ts) - n - m + 1, by = n + m)
  for (start_index in start_indices) {
    end_index <- start_index + n - 1
    
    # Check index range to ensure it does not exceed data length
    if ((end_index + m) > length(log_vwap_ts)) break
    
    # Current training set segment
    train_series <- window(log_vwap_ts, start = start_index, end = end_index)
    
    # Build SARIMA model
    model <- auto.arima(train_series, seasonal = TRUE)
    
    # Forecast the next m points
    forecast_result <- forecast(model, h = m)
    
    # Store the back-transformed predicted values and actual values
    predicted <- c(predicted, exp(as.numeric(forecast_result$mean)))
    actual <- c(actual, exp(as.numeric(log_vwap_ts[(end_index + 1):(end_index + m)])))
  }
  
  # Calculate RMSE and MAPE
  rmse <- sqrt(mean((predicted - actual)^2, na.rm = TRUE))
  mape <- mean(abs((predicted - actual) / actual), na.rm = TRUE) * 100
  
  # Calculate bias (mean of prediction errors)
  bias <- mean(predicted - actual, na.rm = TRUE)
  
  # Calculate variance (variance of prediction errors)
  variance <- var(predicted - actual, na.rm = TRUE)
  
  # Store error results
  rmse_values <- c(rmse_values, rmse)
  mape_values <- c(mape_values, mape)
  bias_values <- c(bias_values, bias)
  variance_values <- c(variance_values, variance)
}

# Create results data frame
error_df <- data.frame(
  Training_Length = n_values,
  RMSE = rmse_values,
  MAPE = mape_values,
  Bias = bias_values,
  Variance = variance_values
)

# Visualize RMSE and MAPE
p1 <- ggplot(error_df, aes(x = Training_Length)) +
  geom_line(aes(y = RMSE, color = "RMSE"), size = 1) +
  geom_line(aes(y = MAPE, color = "MAPE"), size = 1, linetype = "dashed") +
  labs(
    title = "Relationship Between Training Set Length and Errors",
    x = "Training Set Length (n)",
    y = "Error"
  ) +
  scale_color_manual(values = c("RMSE" = "blue", "MAPE" = "red")) +
  theme_minimal()

# Visualize Bias and Variance
p2 <- ggplot(error_df, aes(x = Training_Length)) +
  geom_line(aes(y = Bias, color = "Bias"), size = 1, linetype = "dotted") +
  geom_line(aes(y = Variance, color = "Variance"), size = 1, linetype = "twodash") +
  labs(
    title = "Bias and Variance Tradeoff",
    x = "Training Set Length (n)",
    y = "Metric Value"
  ) +
  scale_color_manual(values = c("Bias" = "green", "Variance" = "purple")) +
  theme_minimal()

print(p1)
print(p2)
