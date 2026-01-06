# ============================================================
# Module 2: Data Preprocessing and Missing Value Treatment
# ============================================================
# This module preprocesses the daily return matrix by
# handling missing values and filtering stocks based on
# data availability. The objective is to ensure a balanced
# and reliable sample for subsequent regression analysis
# and factor construction.
# ============================================================

require(tidyr)

# ------------------------------------------------------------
# Inspect missing values in daily return matrix
# ------------------------------------------------------------

# Count the total number of missing observations for each stock
nan_sum <- apply(ret, 2, function(x) sum(is.na(x))) %>% data.frame()

# Compute the proportion of missing observations for each stock
nan_mean <- apply(ret, 2, function(x) mean(is.na(x))) %>% data.frame()

# Summary statistics of missing values across stocks
nan_sum %>% summary()
# The 3rd quartile threshold (e.g., 416 missing days) is used
# as a cutoff to retain stocks with sufficient data coverage

# ------------------------------------------------------------
# Filter stocks based on missing value threshold
# ------------------------------------------------------------

# Retain stocks with missing observations below the threshold
# This step reduces noise induced by sparse trading histories
new_list <- nan_sum %>%
  filter(nan_sum <= 416) %>%
  rownames()

# Subset return matrix to selected stocks
new_data <- ret %>% select(all_of(new_list))

# ------------------------------------------------------------
# Handle missing values
# ------------------------------------------------------------

# Replace all remaining missing values with zeros
# This assumes that non-trading days correspond to zero returns
# and avoids losing observations in panel regressions
new_data[, -1] <- new_data[, -1] %>%
  apply(2, function(x) replace_na(x, 0))

# Set trading dates as row names
rownames(new_data) <- new_data$Trddt %>% unique()

# ------------------------------------------------------------
# Load estimated conditional volatility series
# ------------------------------------------------------------

# Import conditional volatility estimates obtained
# from an EGARCH model
setwd("D:\\文档\\work_data_2022_12")

sigma_egarch <- read.csv('sigma_egarch.csv')

# Remove automatically generated column prefixes
colnames(sigma_egarch) <- gsub('X', '', colnames(sigma_egarch))

# Set trading dates as row names
rownames(sigma_egarch) <- sigma_egarch[, 1]
sigma_egarch <- sigma_egarch[, -1]

# The conditional volatility series will later be used
# as a control variable in asymmetric feedback trading
# regressions
