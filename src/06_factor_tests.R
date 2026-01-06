# ============================================================
# Module 6: Factor Return Construction and Statistical Tests
# ============================================================
# This module computes factor portfolio returns using
# value-weighted schemes and evaluates their statistical
# significance with Newey–West adjusted t-tests to account
# for heteroskedasticity and autocorrelation.
# ============================================================

library(plyr)

# ------------------------------------------------------------
# Construct daily factor portfolio returns (quintile sorting)
# ------------------------------------------------------------

# Size factor (SMB-style)
smb <- ddply(
  new_data,
  .(trade_date, group_cmktv),
  function(x) sum(x$ret * x$cmktv) / sum(x$cmktv)
)

smb <- reshape2::dcast(smb, trade_date ~ group_cmktv, value.var = "V1")
smb$MINUS <- smb$L - smb$H

# ------------------------------------------------------------
# Liquidity factor (High minus Low)
# ------------------------------------------------------------

Liq_HML <- ddply(
  new_data,
  .(trade_date, group_Liquid),
  function(x) sum(x$ret * x$cmktv) / sum(x$cmktv)
)

Liq_HML <- reshape2::dcast(Liq_HML, trade_date ~ group_Liquid, value.var = "V1")
Liq_HML$MINUS <- Liq_HML$L - Liq_HML$H

# ------------------------------------------------------------
# Value factor (High minus Low)
# ------------------------------------------------------------

V_HML <- ddply(
  new_data,
  .(trade_date, group_PE),
  function(x) sum(x$ret * x$cmktv) / sum(x$cmktv)
)

V_HML <- reshape2::dcast(V_HML, trade_date ~ group_PE, value.var = "V1")
V_HML$MINUS <- V_HML$H - V_HML$L

# ------------------------------------------------------------
# Asymmetric Feedback Trading factor (Low minus High)
# ------------------------------------------------------------

AFT_LMH <- ddply(
  new_data,
  .(trade_date, group_AFT),
  function(x) sum(x$ret * x$cmktv) / sum(x$cmktv)
)

AFT_LMH <- reshape2::dcast(AFT_LMH, trade_date ~ group_AFT, value.var = "V1")
AFT_LMH$MINUS <- AFT_LMH$L - AFT_LMH$H

# ------------------------------------------------------------
# Load market benchmark (CSI 500)
# ------------------------------------------------------------

setwd("D:\\文档\\work_data_2022_12")

mkt <- read.csv('指数收益率.csv')

ret_zz500 <- mkt[c('trade_date', 'pct_chg_ZZ500')]
ret_zz500 <- dplyr::filter(ret_zz500, trade_date %in% AFT_LMH$trade_date)
ret_zz500$pct_chg_ZZ500 <- ret_zz500$pct_chg_ZZ500 / 100
ret_zz500 <- dplyr::arrange(ret_zz500, trade_date)

# ------------------------------------------------------------
# Newey–West adjusted statistical tests
# ------------------------------------------------------------

library(AER)

# Define function to compute Newey–West t-statistics
Test_Newey_West <- function(sample) {
  
  n <- length(rownames(sample))
  
  # Estimate intercept-only regressions
  m1 <- lm(L ~ 1, sample)
  m2 <- lm(`2` ~ 1, sample)
  m3 <- lm(`3` ~ 1, sample)
  m4 <- lm(`4` ~ 1, sample)
  m5 <- lm(H ~ 1, sample)
  mm <- lm(MINUS ~ 1, sample)
  
  # Apply Newey–West covariance correction
  t1 <- coeftest(m1, vcov. = NeweyWest(m1, lag = floor(4 * (n / 100)^(2 / 9)), prewhite = FALSE))
  t2 <- coeftest(m2, vcov. = NeweyWest(m2, lag = floor(4 * (n / 100)^(2 / 9)), prewhite = FALSE))
  t3 <- coeftest(m3, vcov. = NeweyWest(m3, lag = floor(4 * (n / 100)^(2 / 9)), prewhite = FALSE))
  t4 <- coeftest(m4, vcov. = NeweyWest(m4, lag = floor(4 * (n / 100)^(2 / 9)), prewhite = FALSE))
  t5 <- coeftest(m5, vcov. = NeweyWest(m5, lag = floor(4 * (n / 100)^(2 / 9)), prewhite = FALSE))
  tm <- coeftest(mm, vcov. = NeweyWest(mm, lag = floor(4 * (n / 100)^(2 / 9)), prewhite = FALSE))
  
  # Collect results
  t_list <- c(t1, t2, t3, t4, t5, tm)
  
  tmean  <- t_list[seq(1, length(t_list), 4)]
  tstd   <- t_list[seq(2, length(t_list), 4)]
  tvalue <- t_list[seq(3, length(t_list), 4)]
  tp     <- t_list[seq(4, length(t_list), 4)]
  
  tresult <- data.frame(tmean, tstd, tvalue, tp)
  tresult$group <- c('L', '2', '3', '4', 'H', 'MINUS')
  
  return(tresult)
}

# ------------------------------------------------------------
# Apply Newey–West tests to factor portfolios
# ------------------------------------------------------------

sample <- dplyr::filter(
  AFT_LMH,
  trade_date >= '2006-01-01' & trade_date <= '2022-01-01'
)

V_t    <- Test_Newey_West(sample); V_t$factor    <- 'Value'
SIZE_t <- Test_Newey_West(sample); SIZE_t$factor <- 'Size'
Liq_t  <- Test_Newey_West(sample); Liq_t$factor  <- 'Liquidity'
AFT_t  <- Test_Newey_West(sample); AFT_t$factor  <- 'AFT'

newey_west_t <- rbind(V_t, SIZE_t, Liq_t, AFT_t)

# ------------------------------------------------------------
# Benchmark test for CSI 500
# ------------------------------------------------------------

n <- length(rownames(ret_zz500))
m <- lm(pct_chg_ZZ500 ~ 1, ret_zz500)

zz500_t <- coeftest(
  m,
  vcov. = NeweyWest(m, lag = floor(4 * (n / 100)^(2 / 9)), prewhite = FALSE)
)

zz500_t <- as.data.frame(t(data.frame(zz500_t[, 1:4])))
colnames(zz500_t) <- c('tmean', 'tstd', 'tvalue', 'tp')
rownames(zz500_t) <- 25
zz500_t$group <- 'zz500'
zz500_t$factor <- 'zz500'

newey_west_t <- rbind(newey_west_t, zz500_t)

# Save statistical test results
write.csv(newey_west_t, 'newey_west_t.csv')
