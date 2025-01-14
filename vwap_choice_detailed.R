# This script analyzes VWAP time series data aggregated over different time intervals. 
# It calculates statistical metrics such as Ljung-Box, Box-Pierce, and ADF P-values, and compares 
# ARIMA and SARIMA models' AIC values to identify the optimal aggregation interval.

# Install and load required packages
required_packages <- c("data.table", "dplyr", "ggplot2", "forecast", "tseries")
install_missing_packages <- function(packages) {
  missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(missing_packages) > 0) {
    install.packages(missing_packages)
  }
}
install_missing_packages(required_packages)
lapply(required_packages, library, character.only = TRUE)

# Custom VWAP aggregation function
calculate_agg_ratio_and_vwap <- function(data, group_interval) {
  data <- data %>%
    mutate(TimeGroup = floor(TradingTime / group_interval) * group_interval) # Group by custom interval
  
  agg_ratio <- data %>%
    group_by(TimeGroup) %>%
    summarise(
      TotalAggBuy = sum(AggressiveBuyVolume, na.rm = TRUE),
      TotalAggSell = sum(AggressiveSellVolume, na.rm = TRUE),
      TotalVolume = sum(Size, na.rm = TRUE),
      AggBuySellRatio = TotalAggBuy / (TotalAggBuy + TotalAggSell),
      VWAP = sum(Price * Size, na.rm = TRUE) / sum(Size, na.rm = TRUE),
      StartTime = min(TradingTime),
      .groups = "drop"
    ) %>%
    mutate(
      StartTimeFormatted = format(as.POSIXct(StartTime, origin = "1970-01-01", tz = "UTC"), "%H:%M:%S")
    )
  
  return(agg_ratio)
}

# Custom VWAP aggregation and statistical testing function
calculate_agg_and_test <- function(group_interval, merged_data, use_log_diff = FALSE) {
  # VWAP aggregation
  agg_data <- calculate_agg_ratio_and_vwap(merged_data, group_interval)
  
  # Extract time series data
  time_series <- ts(agg_data$VWAP)
  
  if (use_log_diff) {
    # Log transformation and differencing
    time_series <- log(time_series)
    time_series <- diff(time_series)
  }
  
  # Check data length
  if (length(time_series) < 20) return(NULL)
  
  # Perform decorrelation tests
  ljung_box <- Box.test(time_series, lag = 10, type = "Ljung-Box")
  box_pierce <- Box.test(time_series, lag = 10, type = "Box-Pierce")
  adf <- adf.test(time_series)
  
  # Fit ARIMA models
  arima_fit <- auto.arima(time_series)
  sarima_fit <- auto.arima(time_series, seasonal = TRUE, stepwise = TRUE, approximation = FALSE)
  
  # Return results
  return(list(
    interval = group_interval,
    ljung_box_p = ljung_box$p.value,
    box_pierce_p = box_pierce$p.value,
    adf_p = adf$p.value,
    arima_aic = arima_fit$aic,
    sarima_aic = sarima_fit$aic
  ))
}

# Load and preprocess data
order_book <- fread("GOOG_2012-06-21_34200000_57600000_orderbook_1.csv", col.names = c("SellPrice1", "SellVolume", "BuyPrice1", "BuyVolume"))
trades <- fread("GOOG_2012-06-21_34200000_57600000_message_1.csv", col.names = c("Time", "Type", "OrderID", "Size", "Price", "Direction"))

order_book <- order_book %>%
  mutate(SellPrice1 = SellPrice1 / 10000, BuyPrice1 = BuyPrice1 / 10000)

trades <- trades %>%
  mutate(Price = Price / 10000, TradingTime = Time)

merged_data <- cbind(trades, order_book) %>%
  mutate(
    AggressiveBuyVolume = ifelse(Price >= SellPrice1, Size, 0),
    AggressiveSellVolume = ifelse(Price <= BuyPrice1, Size, 0)
  )

# Test with different time intervals
group_intervals <- seq(10, 600, by = 10) # Aggregation intervals from 10 to 600 seconds
use_log_diff <- TRUE

results <- lapply(group_intervals, calculate_agg_and_test, merged_data = merged_data, use_log_diff = use_log_diff)

# Summarize results
results_df <- do.call(rbind, lapply(results, as.data.frame))

print(results_df)

# Plot changes in metrics
# Ljung-Box P-value curve
ggplot(results_df, aes(x = interval, y = ljung_box_p)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 0.05, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Ljung-Box P-value vs. VWAP Aggregation Interval",
    x = "VWAP Aggregation Interval (seconds)",
    y = "Ljung-Box P-value"
  ) +
  theme_minimal()

# Box-Pierce P-value curve
ggplot(results_df, aes(x = interval, y = box_pierce_p)) +
  geom_line(color = "green") +
  geom_hline(yintercept = 0.05, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Box-Pierce P-value vs. VWAP Aggregation Interval",
    x = "VWAP Aggregation Interval (seconds)",
    y = "Box-Pierce P-value"
  ) +
  theme_minimal()

# ADF P-value curve
ggplot(results_df, aes(x = interval, y = adf_p)) +
  geom_line(color = "red") +
  geom_hline(yintercept = 0.05, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "ADF P-value vs. VWAP Aggregation Interval",
    x = "VWAP Aggregation Interval (seconds)",
    y = "ADF P-value"
  ) +
  theme_minimal()

# ARIMA AIC curve
ggplot(results_df, aes(x = interval, y = arima_aic)) +
  geom_line(color = "purple") +
  labs(
    title = "ARIMA AIC vs. VWAP Aggregation Interval",
    x = "VWAP Aggregation Interval (seconds)",
    y = "ARIMA AIC"
  ) +
  theme_minimal()

# SARIMA AIC curve
ggplot(results_df, aes(x = interval, y = sarima_aic)) +
  geom_line(color = "orange") +
  labs(
    title = "SARIMA AIC vs. VWAP Aggregation Interval",
    x = "VWAP Aggregation Interval (seconds)",
    y = "SARIMA AIC"
  ) +
  theme_minimal()

# Combine Ljung-Box, Box-Pierce, and ADF P-value into one plot
ggplot(results_df, aes(x = interval)) +
  geom_line(aes(y = ljung_box_p, color = "Ljung-Box P-value"), size = 1) +
  geom_line(aes(y = box_pierce_p, color = "Box-Pierce P-value"), size = 1) +
  geom_line(aes(y = adf_p, color = "ADF P-value"), size = 1) +
  geom_hline(yintercept = 0.05, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "P-values vs. VWAP Aggregation Interval",
    x = "VWAP Aggregation Interval (seconds)",
    y = "P-value",
    color = "Metric"
  ) +
  scale_color_manual(values = c(
    "Ljung-Box P-value" = "blue",
    "Box-Pierce P-value" = "green",
    "ADF P-value" = "purple"
  )) +
  theme_minimal() +
  theme(legend.position = "top")

# Find the optimal aggregation interval
optimal_interval <- results_df[which.min(results_df$arima_aic), "interval"]
cat("Optimal VWAP aggregation interval:", optimal_interval, "seconds\n")
