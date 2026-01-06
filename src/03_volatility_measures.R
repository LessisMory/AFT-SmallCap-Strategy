# ============================================================
# Module 3: Volatility Measures (Realized Volatility)
# ============================================================
# This module constructs realized volatility (RV) measures
# from daily data and aggregates them to the monthly level.
# Realized volatility serves as a non-parametric proxy for
# time-varying risk and is later used in asymmetric feedback
# trading regressions.
# ============================================================

# ------------------------------------------------------------
# Load realized volatility data
# ------------------------------------------------------------

# Identify directories containing realized volatility files
dirs <- list.files()
path_list <- subset(dirs, grepl('已实现', dirs))

# Import and concatenate realized volatility data
df <- import_csv_files(path_list)

# Filter stocks based on predefined universe
data <- df[which(df$Stkcd %in% stock_list), ]

# ------------------------------------------------------------
# Reshape realized volatility data into wide format
# ------------------------------------------------------------

# Convert daily realized volatility into a wide matrix
# Rows represent trading dates, columns represent stocks
RV <- reshape2::dcast(
  data,
  Trddt ~ Stkcd,
  value.var = "RV"
)

# ------------------------------------------------------------
# Convert wide format to long format
# ------------------------------------------------------------

# Transform realized volatility data into long format
# to facilitate panel regression and merging operations
RV <- reshape2::melt(
  RV,
  id.vars = 'Trddt',
  variable.name = 'ts_code',
  value.name = 'RV'
)

# Standardize column names
colnames(RV) <- c("trade_date", "ts_code", "RV")

# Convert stock codes to integer format
RV$ts_code <- RV$ts_code %>%
  sapply(function(x) x %>% as.character() %>% as.integer())

# ------------------------------------------------------------
# Handle missing values in realized volatility
# ------------------------------------------------------------

# Inspect missing values across variables
nan_sum <- apply(RV, 2, function(x) sum(is.na(x))) %>% data.frame()

# Replace missing realized volatility values with zeros
# This avoids dropping observations in subsequent regressions
setnafill(RV, "const", cols = c('RV'), fill = 0)

# ------------------------------------------------------------
# Aggregate realized volatility to monthly frequency
# ------------------------------------------------------------

# Extract year-month identifier from trading dates
RV$month <- RV[, 'trade_date'] %>%
  sapply(function(x) substr(x, 1, 7))

# Convert to data.table for efficient manipulation
RV <- data.table(RV)

# At this stage, realized volatility is available at the
# daily level with a corresponding monthly identifier.
# Monthly aggregation will be implicitly handled in
# rolling-window regressions in later modules.
