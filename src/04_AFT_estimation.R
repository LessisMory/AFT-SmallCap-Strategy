# ============================================================
# Module 4: Asymmetric Feedback Trading (AFT) Estimation
# ============================================================
# This module estimates asymmetric feedback trading intensity
# using rolling-window regressions. The AFT measure captures
# asymmetric investor responses to past returns interacted
# with time-varying volatility.
# ============================================================

# ------------------------------------------------------------
# Construct lagged returns and indicator variables
# ------------------------------------------------------------

# Extract contemporaneous daily returns r_t
r_t <- new_data[2:nrow(new_data), ]

# Construct one-period lagged returns r_{t-1}
r_t_1 <- new_data[1:nrow(new_data) - 1, ]

# Align row names and trading dates
rownames(r_t_1) <- rownames(r_t)
r_t_1$Trddt <- r_t$Trddt

# Construct indicator variable I{r_{t-1} > 0}
# This dummy captures asymmetric responses to positive returns
dummy_r <- apply(
  r_t_1[, -1],
  2,
  function(x) sapply(x, function(x) if (x > 0) 1 else 0)
)

# Align realized volatility series
RV <- RV[-1, ]

# ------------------------------------------------------------
# Reshape data from wide to long format
# ------------------------------------------------------------

# Convert r_t into long format
r_t <- reshape2::melt(r_t)
colnames(r_t) <- c("trade_date", "ts_code", "r_t")
r_t <- as.data.table(r_t, keep.rownames = FALSE)

r_t$ts_code <- r_t$ts_code %>%
  sapply(function(x) x %>% as.character() %>% as.integer())

# Convert r_{t-1} into long format
r_t_1 <- reshape2::melt(r_t_1)
colnames(r_t_1) <- c("trade_date", "ts_code", "r_t_1")
r_t_1 <- as.data.table(r_t_1, keep.rownames = FALSE)

r_t_1$ts_code <- r_t_1$ts_code %>%
  sapply(function(x) x %>% as.character() %>% as.integer())

# Convert dummy variable into long format
dummy_r <- reshape2::melt(dummy_r)
colnames(dummy_r) <- c("trade_date", "ts_code", "dummy_r")
dummy_r <- as.data.table(dummy_r, keep.rownames = FALSE)

# ------------------------------------------------------------
# Merge all variables into a single regression dataset
# ------------------------------------------------------------

# Combine returns, lagged returns, indicator variables,
# and realized volatility into a unified panel dataset
data_lm <- Reduce(
  function(x, y) merge(x, y, by = c('ts_code', 'trade_date'), all = FALSE),
  list(r_t, r_t_1, dummy_r, RV)
)

# ------------------------------------------------------------
# Generate year and month identifiers
# ------------------------------------------------------------

library(lubridate)

data_lm[, 'year'] <- data_lm[, 'trade_date'] %>%
  sapply(function(x) substr(x, 1, 4))

data_lm[, 'month'] <- data_lm[, 'trade_date'] %>%
  sapply(function(x) substr(x, 1, 7))

# Save regression-ready dataset
write.csv(data_lm, 'data_lm_zz500.csv')

# ------------------------------------------------------------
# Define function to estimate AFT via linear regression
# ------------------------------------------------------------

# The asymmetric feedback trading coefficient (AFT)
# corresponds to the interaction term:
# r_{t-1} × I{r_{t-1} > 0} × volatility
cal_AFT <- function(data, formula) {
  
  fit <- lm(formula = formula, data = data)
  
  # Extract the coefficient associated with asymmetric trading
  aft <- coef(fit)[[6]]
  
  return(aft)
}

library(plyr)
library(rollRegres)

# ------------------------------------------------------------
# Rolling-window estimation of monthly AFT
# ------------------------------------------------------------

# Estimate AFT using a rolling window of 12 months
# Each month’s AFT is estimated using daily observations
# within the past one-year window
cal_AFTs_by_Month <- function(data, formula) {
  
  st <- unique(data$ts_code)
  
  # Convert month identifiers into group indices
  date <- data$month %>% as.factor() %>% as.integer()
  
  # Rolling regression with monthly grouping
  roll_fit <- roll_regres(
    formula,
    data,
    width = 12L,
    grp = date,
    min_obs = 13L,
    do_downdates = FALSE
  )
  
  # Extract valid AFT coefficients
  AFT <- unique(na.omit(roll_fit$coefs)[, 6])
  
  # Corresponding month labels
  month <- unique(data$month)[-c(1:11)]
  
  result <- data.table(month, AFT)
  
  cat('[INFO]', as.character(Sys.time()),
      'AFT estimation completed for stock:', st, '\n')
  
  return(result)
}

# ------------------------------------------------------------
# Model specification for AFT estimation
# ------------------------------------------------------------

# Mean equation with asymmetric feedback trading:
#
# r_t = β0
#     + β1 r_{t-1}
#     + β2 r_{t-1} I{r_{t-1} > 0}
#     + β3 RV_t
#     + β4 r_{t-1} × RV_t
#     + β5 r_{t-1} I{r_{t-1} > 0} × RV_t
#     + ε_t
#
# The coefficient β5 captures asymmetric feedback trading intensity

formula_RV <- r_t ~ r_t_1 + r_t_1:dummy_r +
  RV + r_t_1:RV + r_t_1:dummy_r:RV

# Estimate rolling monthly AFT for each stock
AFTs_RV <- ddply(data_lm, .(ts_code),
                 cal_AFTs_by_Month, formula_RV)

# Rename AFT column
colnames(AFTs_RV)[3] <- 'AFT_RV'
