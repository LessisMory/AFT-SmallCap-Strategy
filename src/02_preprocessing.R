# ============================================================
# Data Preprocessing
# ============================================================

require(tidyr)

# ------------------------------------------------------------
# Missing value diagnostics for daily return matrix
# ------------------------------------------------------------

# Count the number of missing observations for each stock
nan_sum <- apply(ret, 2, function(x) sum(is.na(x))) %>% data.frame()

# Compute the proportion of missing observations for each stock
nan_mean <- apply(ret, 2, function(x) mean(is.na(x))) %>% data.frame()

# ------------------------------------------------------------
# Sample filtering based on missing data threshold
# ------------------------------------------------------------

# Retain stocks with acceptable data availability
# Threshold: at most 416 missing daily observations
new_list <- nan_sum %>% filter(nan_sum <= 416) %>% rownames()

# Subset return matrix to selected stocks
new_data <- ret %>% select(all_of(new_list))

# ------------------------------------------------------------
# Missing value treatment
# ------------------------------------------------------------

# Replace missing daily returns with zero
# (common practice in empirical asset pricing when missing days
# correspond to non-trading or suspended observations)
new_data[, -1] <- new_data[, -1] %>%
  apply(2, function(x) replace_na(x, 0))

# Set row names to unique trading dates
rownames(new_data) <- new_data$Trddt %>% unique()

# ------------------------------------------------------------
# Prepare lagged returns for factor construction
# ------------------------------------------------------------

# Current-period returns
r_t <- new_data[2:nrow(new_data), ]

# Lagged returns (one-day lag)
r_t_1 <- new_data[1:nrow(new_data) - 1, ]

# Align row names between current and lagged returns
rownames(r_t_1) <- rownames(r_t)
r_t_1$Trddt <- r_t$Trddt

# ------------------------------------------------------------
# Construct indicator for positive lagged returns
# ------------------------------------------------------------

# Dummy variable: equals 1 if lagged return is positive, otherwise 0
dummy_r <- apply(
  r_t_1[, -1],
  2,
  function(x) sapply(x, function(x) if (x > 0) 1 else 0)
)

# ------------------------------------------------------------
# Reshape data from wide format to long format
# ------------------------------------------------------------

# Convert current returns to long format
r_t <- reshape2::melt(r_t)
colnames(r_t) <- c("trade_date", "ts_code", "r_t")
r_t <- as.data.table(r_t)

# Convert lagged returns to long format
r_t_1 <- reshape2::melt(r_t_1)
colnames(r_t_1) <- c("trade_date", "ts_code", "r_t_1")
r_t_1 <- as.data.table(r_t_1)

# Convert dummy indicator to long format
dummy_r <- reshape2::melt(dummy_r)
colnames(dummy_r) <- c("trade_date", "ts_code", "dummy_r")
dummy_r <- as.data.table(dummy_r)
