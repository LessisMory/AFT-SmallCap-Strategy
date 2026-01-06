# ============================================================
# Module 7: Strategy Backtesting and Performance Evaluation
# ============================================================
# This module implements a factor-based trading strategy
# using rolling regressions to estimate factor exposures
# and predict future returns. The strategy is evaluated
# using cumulative returns, Sharpe ratio, and maximum drawdown.
# ============================================================

# ------------------------------------------------------------
# Merge factor portfolio returns into individual stock returns
# ------------------------------------------------------------

# Combine individual stock returns with factor returns
daily_return <- new_data[, c(1:4)]

daily_return <- Reduce(
  function(x, y) merge(x, y, by = c('trade_date'), all = FALSE),
  list(
    daily_return,
    smb[, c('trade_date', 'L')],
    Liq_HML[, c('trade_date', '4')],
    V_HML[, c('trade_date', '2')],
    AFT_LMH[, c('trade_date', 'L')]
  )
)

# Rename factor return columns
colnames(daily_return)[5:8] <- c('SIZE_L', 'LIQ_4', 'V_2', 'AFT_L')

# ------------------------------------------------------------
# Define backtesting period
# ------------------------------------------------------------

# Specify backtesting start and end dates
start_date <- '2021-01-01'
end_date   <- '2022-01-01'

# Construct estimation window (including pre-sample period)
total_date <- unique(daily_return$trade_date)

date_list <- c(
  subset(total_date, total_date < start_date) %>% tail(500),
  subset(total_date, total_date >= start_date & total_date <= end_date)
)

# Filter sample for backtesting
trade_ret <- subset(daily_return, trade_date %in% date_list)

# ------------------------------------------------------------
# Define rebalancing frequency (weekly)
# ------------------------------------------------------------

# Rebalance every 5 trading days
gap_day <- 5
trade_length <- length(date_list)

# Create rebalancing indicators
gap_mark <- c(
  rep(1:floor(trade_length / gap_day), each = gap_day),
  rep(floor(trade_length / gap_day) + 1, each = trade_length %% gap_day)
) %>% as.integer()

trade_date <- data.frame(date_list, gap_mark)
colnames(trade_date) <- c('trade_date', 'gap_mark')

trade_ret <- merge(trade_ret, trade_date, by = c('trade_date'), all = FALSE)

# ------------------------------------------------------------
# Estimate rolling factor loadings (weekly frequency)
# ------------------------------------------------------------

library(rollRegres)

# Function to estimate rolling factor exposures
Cal_Loadings_by_Week <- function(data, formula) {
  
  st <- unique(data$ts_code)
  gap_mark <- sort(data$gap_mark)
  
  # Rolling regression over a 50-week window
  roll_fit <- roll_regres(
    formula,
    data,
    width = 50L,
    grp = gap_mark,
    min_obs = 5L,
    do_downdates = FALSE
  )
  
  coefs <- unique(na.omit(roll_fit$coefs)) %>% data.table()
  coefs$gap_mark <- sort(unique(data$gap_mark))[-c(1:49)]
  
  cat('[INFO]', as.character(Sys.time()),
      'Factor loadings estimated for stock:', st, '\n')
  
  return(coefs)
}

# Regression model for factor exposure estimation
formula <- ret ~ SIZE_L + LIQ_4 + V_2 + AFT_L

loadings <- ddply(trade_ret, .(ts_code),
                  Cal_Loadings_by_Week, formula)

# ------------------------------------------------------------
# Compute weekly stock returns
# ------------------------------------------------------------

week_ret <- ddply(
  trade_ret,
  .(ts_code, gap_mark),
  function(x) as.vector(cumprod(1 + x$ret) - 1) %>% tail(1)
)

colnames(week_ret)[3] <- 'week_ret'

gap_data <- merge(loadings, week_ret,
                  by = c('ts_code', 'gap_mark'), all = FALSE)

# ------------------------------------------------------------
# Train predictive model using lagged factor exposures
# ------------------------------------------------------------

# Shift weekly returns by one period
last_gap <- sort(unique(gap_data$gap_mark)) %>% tail(1)
fit_data <- subset(gap_data, gap_mark != last_gap)
fit_data$gap_mark <- (fit_data$gap_mark + 1) %>% as.integer()

formula <- week_ret ~ SIZE_L + LIQ_4 + V_2 + AFT_L

fit <- ddply(fit_data, .(ts_code),
             Cal_Loadings_by_Week, formula)

# Rename regression coefficients
colnames(fit)[2:6] <- c('b0', 'b1', 'b2', 'b3', 'b4')

# ------------------------------------------------------------
# Predict future weekly returns
# ------------------------------------------------------------

pred_data <- merge(loadings, fit,
                   by = c('ts_code', 'gap_mark'), all = FALSE)

pred_data$Pred_ret <- pred_data$b0 +
  pred_data$SIZE_L * pred_data$b1 +
  pred_data$LIQ_4  * pred_data$b2 +
  pred_data$V_2    * pred_data$b3 +
  pred_data$AFT_L  * pred_data$b4

pred_data <- pred_data[, c('ts_code', 'gap_mark', 'Pred_ret')]

# ------------------------------------------------------------
# Stock selection based on predicted returns
# ------------------------------------------------------------

# Select top decile stocks by predicted returns
pred_data <- pred_data %>%
  group_by(gap_mark) %>%
  mutate(
    Pred_group = cut(
      Pred_ret,
      quantile(Pred_ret, seq(0, 1, by = 0.1)),
      include.lowest = TRUE,
      labels = 1:10
    )
  ) %>% ungroup()

final_stock <- subset(pred_data, Pred_group == 10)
write.csv(final_stock, 'final_stock.csv')

# ------------------------------------------------------------
# Portfolio performance evaluation
# ------------------------------------------------------------

# Compute daily portfolio returns using value-weighted scheme
port_ret <- ddply(
  hold_ret,
  .(trade_date),
  function(x) sum(x$ret * x$cmktv) / sum(x$cmktv)
)

colnames(port_ret)[2] <- 'ret'

# Adjust for transaction costs
port_ret$ret <- port_ret$ret - 0.001

# Compute cumulative returns
port_ret$cum_ret <- (cumprod(1 + port_ret$ret) - 1) * 100

# ------------------------------------------------------------
# Performance metrics
# ------------------------------------------------------------

# Sharpe ratio
sharp_ratio <- mean(port_ret$ret) / sd(port_ret$ret)

# Maximum drawdown
draw_down <- c()
n <- length(rownames(port_ret))

for (i in 1:n) {
  current <- port_ret[rownames(port_ret)[i], 'cum_ret']
  peak <- max(port_ret[c(1:i), 'cum_ret'])
  draw_down <- append(draw_down, (peak / current - 1) * 100)
}

cat('[INFO]', as.character(Sys.time()),
    'Cumulative return:', tail(port_ret$cum_ret, 1), '%\n')
cat('[INFO]', as.character(Sys.time()),
    'Sharpe ratio:', sharp_ratio, '\n')
cat('[INFO]', as.character(Sys.time()),
    'Maximum drawdown:', max(draw_down), '%\n')
