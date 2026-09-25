# ==============================================================================
# BDA400 - Technical Analysis using R (Stage 1)
# File: technical_analysis_functions.R
# Description: Functions to load stock data, compute stats, and display output
# ==============================================================================

# Step 1: Load / Install Required Packages
required_packages <- c("quantmod", "TTR", "dplyr", "ggplot2", "readr")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(quantmod)
library(TTR)
library(dplyr)
library(ggplot2)

# Helper function to calculate mode
get_mode <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# ------------------------------------------------------------------------------
# Function 1: Load Stock Data from portfolio.txt
# ------------------------------------------------------------------------------
load_stock_data <- function(file_path = "portfolio.txt", from_date = "2023-01-01") {
  if(!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }
  
  symbols <- readLines(file_path)
  symbols <- trimws(symbols)
  symbols <- symbols[symbols != ""]
  
  stock_data_list <- list()
  
  for(symbol in symbols) {
    cat("Downloading data for:", symbol, "...\n")
    tryCatch({
      # Get data using quantmod
      data <- getSymbols(symbol, src = "yahoo", from = from_date, auto.assign = FALSE)
      stock_data_list[[symbol]] <- data
    }, error = function(e) {
      cat("Failed to download data for", symbol, ":", e$message, "\n")
    })
  }
  
  return(stock_data_list)
}

# ------------------------------------------------------------------------------
# Function 2: Calculate Statistics (MA, Mean, Mode, Median, Std Dev)
# ------------------------------------------------------------------------------
calculate_statistics <- function(stock_xts, symbol = "STOCK") {
  # Extract Adjusted Close prices
  adj_close <- AsNumeric(Quantmod::Ad(stock_xts))
  adj_close <- na.omit(adj_close)
  
  # Basic Statistics
  stats_summary <- data.frame(
    Symbol = symbol,
    Mean = round(mean(adj_close), 2),
    Median = round(median(adj_close), 2),
    Mode = round(get_mode(adj_close), 2),
    Std_Dev = round(sd(adj_close), 2),
    Min = round(min(adj_close), 2),
    Max = round(max(adj_close), 2)
  )
  
  # 20-Day Simple Moving Average using TTR
  sma_20 <- SMA(Quantmod::Ad(stock_xts), n = 20)
  
  return(list(
    SummaryStats = stats_summary,
    SMA20 = sma_20
  ))
}

# ------------------------------------------------------------------------------
# Function 3: Display Loaded Data and Statistics
# ------------------------------------------------------------------------------
display_stock_analysis <- function(stock_data_list) {
  all_stats <- list()
  
  for(symbol in names(stock_data_list)) {
    stock_xts <- stock_data_list[[symbol]]
    
    cat("\n============================================")
    cat("\nMarket Data Summary for:", symbol)
    cat("\n============================================\n")
    print(head(stock_xts, 5)) # Show first 5 rows
    
    analysis <- calculate_statistics(stock_xts, symbol)
    
    cat("\nCalculated Statistics:\n")
    print(analysis$SummaryStats)
    
    all_stats[[symbol]] <- analysis$SummaryStats
  }
  
  # Combine into a single comparison table
  combined_df <- do.call(rbind, all_stats)
  cat("\n\n============================================")
  cat("\nPortfolio Comparison Table")
  cat("\n============================================\n")
  print(combined_df)
  
  return(combined_df)
}
