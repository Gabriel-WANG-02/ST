# This script performs descriptive statistics and analysis on the VWAP time series data, 
# including statistical summaries, visualization, and stationarity testing.

# Install and load required packages
required_packages <- c("forecast", "moments")
install_missing_packages <- function(packages) {
  missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(missing_packages) > 0) {
    install.packages(missing_packages)
  }
}
install_missing_packages(required_packages)
lapply(required_packages, library, character.only = TRUE)

# Read the input data
input_file <- "agg_ratio_and_vwap_with_volume.csv"
time_series_data <- read.csv(input_file)

# Convert VWAP column to a time series object
vwap_ts <- ts(time_series_data$VWAP)

# 1. Basic Descriptive Statistics
vwap_summary <- summary(vwap_ts)
cat("Summary Statistics:\n")
print(vwap_summary)

# 2. Additional Descriptive Measures
vwap_mean <- mean(vwap_ts, na.rm = TRUE)
vwap_sd <- sd(vwap_ts, na.rm = TRUE)
vwap_min <- min(vwap_ts, na.rm = TRUE)
vwap_max <- max(vwap_ts, na.rm = TRUE)
vwap_skewness <- skewness(vwap_ts, na.rm = TRUE)
vwap_kurtosis <- kurtosis(vwap_ts, na.rm = TRUE)

cat("\nAdditional Measures:\n")
cat("Mean:", vwap_mean, "\n")
cat("Standard Deviation:", vwap_sd, "\n")
cat("Minimum:", vwap_min, "\n")
cat("Maximum:", vwap_max, "\n")
cat("Skewness:", vwap_skewness, "\n")
cat("Kurtosis:", vwap_kurtosis, "\n")

# 3. Time Series Plot
plot(vwap_ts, main = "VWAP Time Series", ylab = "VWAP", xlab = "Time", col = "blue")

# 4. Histogram
hist(vwap_ts, breaks = 30, col = "lightblue", main = "Histogram of VWAP", xlab = "VWAP", ylab = "Frequency")

# 5. Boxplot
boxplot(vwap_ts, main = "Boxplot of VWAP", ylab = "VWAP", col = "lightgreen")

# 6. Autocorrelation Function (ACF)
acf(vwap_ts, main = "Autocorrelation of VWAP", lag.max = 30)

# 7. Time Series Decomposition (if applicable)
# Assuming the time series has a seasonal component, apply decomposition
if (frequency(vwap_ts) > 1) {
  vwap_decomp <- decompose(vwap_ts, type = "additive")
  plot(vwap_decomp, main = "Decomposition of VWAP")
}

# 8. Check for Stationarity (Augmented Dickey-Fuller Test)
library(tseries)
adf_test <- adf.test(vwap_ts, alternative = "stationary")
cat("\nADF Test:\n")
print(adf_test)

# Save output to a file
output_file <- "vwap_descriptive_stats.txt"
sink(output_file)
cat("Summary Statistics:\n")
print(vwap_summary)
cat("\nAdditional Measures:\n")
cat("Mean:", vwap_mean, "\n")
cat("Standard Deviation:", vwap_sd, "\n")
cat("Minimum:", vwap_min, "\n")
cat("Maximum:", vwap_max, "\n")
cat("Skewness:", vwap_skewness, "\n")
cat("Kurtosis:", vwap_kurtosis, "\n")
cat("\nADF Test:\n")
print(adf_test)
sink()
