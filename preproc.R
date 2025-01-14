# Script for analyzing financial order book and trade data, including data preprocessing, metric calculations, and visualization.

# Install and load required packages
required_packages <- c("data.table", "dplyr", "ggplot2")
install_missing_packages <- function(packages) {
  missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(missing_packages) > 0) {
    install.packages(missing_packages)
  }
}
install_missing_packages(required_packages)
lapply(required_packages, library, character.only = TRUE)

# Load data
# order_book <- fread("AMZN_2012-06-21_34200000_57600000_orderbook_1.csv", col.names = c("SellPrice1", "SellVolume", "BuyPrice1", "BuyVolume"))
# trades <- fread("AMZN_2012-06-21_34200000_57600000_message_1.csv", col.names = c("Time", "Type", "OrderID", "Size", "Price", "Direction"))

order_book <- fread("AAPL_2012-06-21_34200000_57600000_orderbook_1.csv", col.names = c("SellPrice1", "SellVolume", "BuyPrice1", "BuyVolume"))
trades <- fread("AAPL_2012-06-21_34200000_57600000_message_1.csv", col.names = c("Time", "Type", "OrderID", "Size", "Price", "Direction"))

# Data preprocessing: convert price columns from integers to floats
order_book <- order_book %>%
  mutate(SellPrice1 = SellPrice1 / 10000,
         BuyPrice1 = BuyPrice1 / 10000)

# Convert timestamps to readable format
trades <- trades %>%
  mutate(
    Price = Price / 10000,
    TradingTime = Time,
    TimeFormatted = format(as.POSIXct(TradingTime, origin = "1970-01-01", tz = "UTC"), "%H:%M:%S")
  )

# Merge trades and order book data
merged_data <- cbind(trades, order_book)

# Define aggressive buy and sell volumes
merged_data <- merged_data %>%
  mutate(AggressiveBuyVolume = ifelse(Price >= SellPrice1, Size, 0),
         AggressiveSellVolume = ifelse(Price <= BuyPrice1, Size, 0))

# Aggregate data by custom time intervals and calculate VWAP and other metrics
calculate_agg_ratio_and_vwap <- function(data, group_interval = 10) {
  data <- data %>%
    mutate(TimeGroup = floor(TradingTime / group_interval) * group_interval)
  
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

# Example: aggregate data every 300 seconds
group_interval <- 300
agg_ratio_and_vwap <- calculate_agg_ratio_and_vwap(merged_data, group_interval)

# Calculate Spearman correlation
spearman_correlation <- cor(agg_ratio_and_vwap$AggBuySellRatio, agg_ratio_and_vwap$VWAP, method = "spearman", use = "complete.obs")
cat(paste("Spearman correlation coefficient:", spearman_correlation, "\n"))

# Ensure no missing values in the aggregated data
agg_ratio_and_vwap <- agg_ratio_and_vwap %>% 
  filter(!is.na(AggBuySellRatio) & !is.na(VWAP))

# Plot aggressive buy/sell ratio over time
ggplot(agg_ratio_and_vwap, aes(x = StartTimeFormatted, y = AggBuySellRatio, group = 1)) +
  geom_line(color = "blue") +
  labs(
    title = paste("Aggressive Buy/Sell Ratio Over Time (every", group_interval, "seconds)"),
    x = "Trading Time",
    y = "Aggressive Buy/Sell Ratio"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = agg_ratio_and_vwap$StartTimeFormatted[seq(1, nrow(agg_ratio_and_vwap), length.out = 6)])

# Plot VWAP over time
ggplot(agg_ratio_and_vwap, aes(x = StartTimeFormatted, y = VWAP, group = 1)) +
  geom_line(color = "red") +
  labs(
    title = paste("VWAP Over Time (every", group_interval, "seconds)"),
    x = "Trading Time",
    y = "VWAP"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = agg_ratio_and_vwap$StartTimeFormatted[seq(1, nrow(agg_ratio_and_vwap), length.out = 6)])

# Plot total volume over time
ggplot(agg_ratio_and_vwap, aes(x = StartTimeFormatted, y = TotalVolume, group = 1)) +
  geom_line(color = "green") +
  labs(
    title = paste("Total Volume Over Time (every", group_interval, "seconds)"),
    x = "Trading Time",
    y = "Total Volume"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = agg_ratio_and_vwap$StartTimeFormatted[seq(1, nrow(agg_ratio_and_vwap), length.out = 6)])

# Save the aggregated data to a CSV file
output_file <- "agg_ratio_and_vwap_with_volume.csv"
write.csv(agg_ratio_and_vwap, file = output_file, row.names = FALSE)
cat(paste("Aggregated data with total volume saved to file:", output_file, "\n"))

cat(paste("Spearman correlation coefficient:", spearman_correlation, "\n"))
