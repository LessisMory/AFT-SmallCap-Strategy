# ============================================================
# Asymmetric Feedback Trading (AFT) Factor Construction
# ============================================================

# ------------------------------------------------------------
# Merge return, lagged return, dummy indicator, and RV data
# ------------------------------------------------------------

# Remove the first observation to align with lagged returns
RV <- RV[-1, ]

# Merge regression dataset by stock code and trade date
data_lm <- Reduce(
  function(x, y) merge(x, y, by = c('ts_code', 'trade_date'), all = FALSE),
  list(r_t, r_t_1, dummy_r, RV)
)

# Extract year and month identifiers for rolling regression
data_lm[, 'year'] <- substr(data_lm$trade_date, 1, 4)
data_lm[, 'month'] <- substr(data_lm$trade_date, 1, 7)

# ------------------------------------------------------------
# Rolling regression specification
# ------------------------------------------------------------

# Model specification:
#
# r_t = β0 
#     + β1 · r_{t-1}
#     + β2 · r_{t-1} · I(r_{t-1} > 0)
#     + β3 · RV_t
#     + β4 · r_{t-1} · RV_t
#     + β5 · r_{t-1} · I(r_{t-1} > 0) · RV_t
#     + ε_t
#
# where β5 captures asymmetric positive feedback trading
# conditional on realized volatility.

library(plyr)
library(rollRegres)

# ------------------------------------------------------------
# Function to estimate rolling AFT coefficients by stock
# ------------------------------------------------------------

cal_AFTs_by_Month <- function(data, formula) {

  # Identify stock code
  st <- unique(data$ts_code)

  # Convert month identifier to integer group index
  date <- as.integer(as.factor(data$month))

  # Perform rolling regression with a 12-month window
  roll_fit <- roll_regres(
    formula,
    data,
    width = 12L,
    grp = date,
    min_obs = 13L,
    do_downdates = FALSE
  )

  # Extract the coefficient corresponding to the asymmetric
  # feedback interaction term (β5)
  AFT <- unique(na.omit(roll_fit$coefs)[, 6])

  # Align estimated coefficients with corresponding months
  month <- unique(data$month)[-c(1:11)]
  result <- data.table(month, AFT)

  # Logging
  cat('[INFO]', as.character(Sys.time()),
      'AFT estimation completed for stock:', st, '\n')

  return(result)
}

# ------------------------------------------------------------
# Estimate AFT factor for each stock
# ------------------------------------------------------------

# Regression formula including asymmetric and volatility interactions
formula_RV <- r_t ~ r_t_1 + r_t_1:dummy_r +
  RV + r_t_1:RV + r_t_1:dummy_r:RV

# Apply rolling regression stock by stock
AFTs_RV <- ddply(data_lm, .(ts_code),
                 cal_AFTs_by_Month, formula_RV)

# Rename coefficient column
colnames(AFTs_RV)[3] <- 'AFT_RV'
