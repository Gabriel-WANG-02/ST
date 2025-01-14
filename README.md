# VWAP Time Series Analysis

This project contains R scripts for analyzing time series data focused on VWAP (Volume Weighted Average Price). The project utilizes data from LOBSTER (Limit Order Book System – The Efficient Reconstructor) to explore and analyze financial time series. 

## Data
- **Note:** This project does not include the `.csv` data files due to space constraints. 
- You can download the required data files for free from the LOBSTER website: [LOBSTER Data Samples](https://lobsterdata.com/info/DataSamples.php).
- The data represents order book and trade message data for various stocks.

## How to Run the Code
1. Use **RStudio** to create a new project in this folder.
2. Download the required raw data from the LOBSTER website and place the `.csv` files in the same directory as the R scripts.
3. Install all required R packages as prompted in the scripts.
4. Run `preproc.R` to preprocess the raw CSV data from LOBSTER. 
   - **Input**: The raw CSV files downloaded from LOBSTER, e.g., **`GOOG_2012-06-21_34200000_57600000_message_1.csv`** and **`GOOG_2012-06-21_34200000_57600000_orderbook_1.csv`**.
   - **Output**: A preprocessed time series file, `agg_ratio_and_vwap_with_volume.csv`.

5. All other R scripts use `agg_ratio_and_vwap_with_volume.csv` as the input for further analysis.

## Explanation of R Scripts
1. **`preproc.R`**: Preprocesses the raw LOBSTER data into a clean time series file (`agg_ratio_and_vwap_with_volume.csv`).
2. **`backtest.R`**: Implements and evaluates backtesting strategies on the VWAP time series.
3. **`bias_variance_tradeoff.R`**: Explores the bias-variance tradeoff in predictive modeling for VWAP.
4. **`descriptive.R`**: Conducts descriptive statistical analysis on the VWAP time series.
5. **`arma_vs_regLin.R`**: Compares ARMA models with linear regression for time series forecasting.
6. **`arma_vs_regLin_valid.R`**: Validates ARMA and linear regression models on testing data.
7. **`stratification_on_len_train.R`**: Tests the impact of training set length on model performance.
8. **`vwap_choice_detailed.R`**: Examines the impact of VWAP aggregation intervals on statistical tests and model performance.
9. **`st.R`**: Performs stationarity testing and transformation on the time series.

## Requirements
- **RStudio** and R installed on your system.
- Required R packages:
  - `forecast`
  - `ggplot2`
  - `data.table`
  - `dplyr`
  - `tseries`
  - `moments`

## Notes
- Ensure that the downloaded CSV files are placed in the same directory as the R scripts for seamless execution.
- Modify file paths in the scripts if you store the data in a different folder.

