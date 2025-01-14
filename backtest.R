# This script implements a SARIMA-based trading strategy using time series data. It generates trade signals, 
# calculates returns, and visualizes cumulative and excess returns for strategy evaluation.

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

# Parameter settings
n <- 40  # Training set length
m <- 2  # Testing set length (number of forecast points)
slippage_rate <- 0.000  # Slippage rate, e.g., 0.01%
trade_cpt <- 0

# Initialize variables
trade_signals <- numeric(length(vwap_ts))  # Initialize trade signals

# Sliding window
start_indices <- seq(1, length(vwap_ts) - n - m + 1, by = m)

for (start_index in start_indices) {
  end_index <- start_index + n - 1
  
  # Check index range to ensure it does not exceed data length
  if ((end_index + m) > length(vwap_ts)) break
  
  # Current training segment
  train_series <- window(vwap_ts, start = start_index, end = end_index)
  
  # Log transformation of training set
  log_train_series <- log(train_series)
  
  # Build SARIMA model
  model <- auto.arima(log_train_series, seasonal = TRUE)
  
  # Forecast the next m points (log scale)
  forecast_result <- forecast(model, h = m)
  predicted_log_prices <- as.numeric(forecast_result$mean)
  
  # Convert predicted values back to the original scale
  predicted_prices <- exp(predicted_log_prices)
  
  # Current price (original scale)
  current_price <- train_series[n]
  
  # Calculate max and min of predicted values (original scale)
  predicted_max <- max(predicted_prices)
  predicted_min <- min(predicted_prices)
  
  max_diff <- abs(predicted_max - current_price)
  min_diff <- abs(predicted_min - current_price)
  
  diff <- max(max_diff, min_diff)
  
  # Update absolute differences list
  if (!exists("absolute_differences")) {
    absolute_differences <- numeric()
  }
  absolute_differences <- c(absolute_differences, diff)
  
  # Calculate current quantile thresholds
  quantile_lower_threshold <- quantile(absolute_differences, 0)
  quantile_upper_threshold <- quantile(absolute_differences, 1)
  
  if (m == 1) {
    if (diff > quantile_lower_threshold && diff <= quantile_upper_threshold) {
      if (max_diff > min_diff) {
        # Buy signal (m=1 only updates current point)
        trade_signals[end_index + 1] <- 1
        trade_cpt <- trade_cpt + 1
      } else if (max_diff < min_diff) {
        # Sell signal (m=1 only updates current point)
        trade_signals[end_index + 1] <- -1
        trade_cpt <- trade_cpt + 1
      }
    } else {
      # No trade signal, signal remains 0
      trade_signals[end_index + 1] <- 0
    }
  } else { # m > 1
    # Determine whether to trade
    if (diff > quantile_lower_threshold && diff <= quantile_upper_threshold) {
      if (max_diff > min_diff) {
        # Buy signal
        trade_signals[(end_index + 1):(end_index + which.max(predicted_prices))] <- 1
        trade_signals[(end_index + which.max(predicted_prices) + 1):(end_index + m)] <- 0
        trade_cpt <- trade_cpt + 1
      } else if (max_diff < min_diff) {
        # Sell signal
        trade_signals[(end_index + 1):(end_index + which.min(predicted_prices))] <- -1
        trade_signals[(end_index + which.min(predicted_prices) + 1):(end_index + m)] <- 0
        trade_cpt <- trade_cpt + 1
      }
    } else {
      # No trade signal, signal remains 0
      trade_signals[(end_index + 1):(end_index + m)] <- 0
    }
  }
}

print(length(vwap_ts))
print(length(trade_signals))

# Calculate returns
if (length(vwap_ts) != length(trade_signals)) {
  trade_signals <- head(trade_signals, length(vwap_ts) - 1)
}

raw_returns <- numeric(length(vwap_ts) - 1)
for (i in 2:length(vwap_ts)) {
  raw_returns[i - 1] <- (vwap_ts[i] - vwap_ts[i - 1]) / vwap_ts[i - 1]
}
strategy_returns <- raw_returns * trade_signals[-length(trade_signals)]

# Calculate cumulative returns
cumulative_strategy_returns <- cumsum(strategy_returns)
cumulative_raw_returns <- cumsum(raw_returns)

# Visualize backtesting results
p1 <- ggplot(data.frame(Time = 1:length(cumulative_strategy_returns), Returns = cumulative_strategy_returns), aes(x = Time, y = Returns)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Cumulative Returns of SARIMA Strategy",
    x = "Time Period",
    y = "Cumulative Returns"
  ) +
  theme_minimal()

p2 <- ggplot(data.frame(Time = 1:length(cumulative_raw_returns), Returns = cumulative_strategy_returns - cumulative_raw_returns), aes(x = Time, y = Returns)) +
  geom_line(color = "red", size = 1) +
  labs(
    title = "Excess Returns (Strategy - Raw Returns)",
    x = "Time Period",
    y = "Excess Returns"
  ) +
  theme_minimal()

# Create data frame
data <- data.frame(
  Time = n:length(cumulative_strategy_returns),
  StrategyReturns = cumulative_strategy_returns[n:length(cumulative_strategy_returns)],
  ExcessReturns = cumulative_strategy_returns[n:length(cumulative_strategy_returns)] - cumulative_raw_returns[n:length(cumulative_strategy_returns)]
)

# Plot the graph
ggplot(data) +
  geom_line(aes(x = Time, y = StrategyReturns, color = "Cumulative Returns of SARIMA Strategy"), size = 1) +
  geom_line(aes(x = Time, y = ExcessReturns, color = "Excess Returns (Strategy - Raw Returns)"), size = 1) +
  labs(
    title = "Cumulative and Excess Returns of SARIMA Strategy",
    x = "Time Period",
    y = "Returns",
    color = "Legend"
  ) +
  scale_color_manual(values = c(
    "Cumulative Returns of SARIMA Strategy" = "blue",
    "Excess Returns (Strategy - Raw Returns)" = "red"
  )) +
  annotate("text", x = max(data$Time) * 0.6, y = max(data$StrategyReturns) * 0.6,
           label = paste("Trade times:", trade_cpt), size = 5, color = "black") +
  theme_minimal()
