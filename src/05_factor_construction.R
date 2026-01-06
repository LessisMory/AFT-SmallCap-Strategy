# ============================================================
# Module 5: Factor Construction and Portfolio Sorting
# ============================================================
# This module constructs firm-level characteristics and
# sorts stocks into portfolios based on size, value,
# liquidity, and asymmetric feedback trading (AFT).
# Monthly portfolio assignments are used for factor
# return calculation and strategy backtesting.
# ============================================================

# ------------------------------------------------------------
# Load valuation and liquidity-related data
# ------------------------------------------------------------

# Set working directory for firm-level characteristic data
setwd("D:\\文档\\work_data_2022_12\\new")

# Identify directories containing daily trading derivatives
dirs <- list.files()
path_list <- subset(dirs, grepl('个股日交易衍生指标', dirs))

# Import valuation-related data
df <- import_csv_files(path_list)

# Retain stocks with estimated AFT measures
value <- dplyr::filter(df, Symbol %in% unique(AFTs_RV$ts_code))

# ------------------------------------------------------------
# Construct market capitalization (Size)
# ------------------------------------------------------------

# Extract circulating market value at daily frequency
cmktv <- reshape2::dcast(
  value,
  TradingDate ~ Symbol,
  value.var = "CirculatedMarketValue"
)

# Convert to long format
cmktv <- reshape2::melt(
  cmktv,
  id.vars = 'TradingDate',
  variable.name = 'ts_code',
  value.name = 'cmktv'
)

colnames(cmktv) <- c("trade_date", "ts_code", "cmktv")

# Standardize stock codes
cmktv$ts_code <- cmktv$ts_code %>%
  sapply(function(x) x %>% as.character() %>% as.integer())

# Fill missing market value observations using last observation carried forward
cmktv <- cmktv %>%
  dplyr::group_by(ts_code) %>%
  fill(cmktv, .direction = "downup") %>%
  dplyr::ungroup()

cmktv <- as.data.table(cmktv, keep.rownames = FALSE)

# ------------------------------------------------------------
# Extract month-end market capitalization
# ------------------------------------------------------------

# Generate month identifiers
cmktv$month <- cmktv[, 'trade_date'] %>%
  sapply(function(x) substr(x, 1, 7))

# Identify the last trading day of each month
month_last_day <- cmktv[, .(last_day = max(trade_date)), by = 'month'][['last_day']]

# Retain month-end observations
cmktv_last <- filter(cmktv, trade_date %in% month_last_day)

# ------------------------------------------------------------
# Construct valuation measure (PE ratio)
# ------------------------------------------------------------

PE <- reshape2::dcast(
  value,
  TradingDate ~ Symbol,
  value.var = "PE"
)

PE <- reshape2::melt(
  PE,
  id.vars = 'TradingDate',
  variable.name = 'ts_code',
  value.name = 'PE'
)

colnames(PE) <- c("trade_date", "ts_code", "PE")

PE$ts_code <- PE$ts_code %>%
  sapply(function(x) x %>% as.character() %>% as.integer())

# Fill missing PE values
PE <- PE %>%
  dplyr::group_by(ts_code) %>%
  fill(PE, .direction = "downup") %>%
  dplyr::ungroup()

PE <- as.data.table(PE, keep.rownames = FALSE)

# Extract month-end PE values
PE$month <- PE[, 'trade_date'] %>%
  sapply(function(x) substr(x, 1, 7))

month_last_day <- PE[, .(last_day = max(trade_date)), by = 'month'][['last_day']]
PE_last <- filter(PE, trade_date %in% month_last_day)

# ------------------------------------------------------------
# Load monthly liquidity measure (Amihud)
# ------------------------------------------------------------

# Import monthly Amihud illiquidity measure
Liquid <- read.csv('个股Amihud指标表(月)155832621\\LIQ_AMIHUD_M.csv')

# Align liquidity data with month-end dates
Liquid <- filter(
  Liquid,
  Trdmnt %in% sapply(month_last_day, function(x) substr(x, 1, 7))
)

Liquid <- filter(Liquid, Stkcd %in% unique(AFTs_RV$ts_code))

Liquid <- as.data.table(Liquid, keep.rownames = FALSE)[, 1:3]
colnames(Liquid) <- c('ts_code', 'month', 'Liquid')

# Fill missing liquidity observations with zeros
Liquid <- reshape2::dcast(Liquid, month ~ ts_code, value.var = "Liquid")
Liquid[, -1] <- Liquid[, -1] %>% apply(2, function(x) replace_na(x, 0))

Liquid <- reshape2::melt(
  Liquid,
  id.vars = 'month',
  variable.name = 'ts_code',
  value.name = 'Liquid'
)

Liquid$ts_code <- Liquid$ts_code %>%
  sapply(function(x) x %>% as.character() %>% as.integer())

# ------------------------------------------------------------
# Merge all firm characteristics
# ------------------------------------------------------------

# Combine AFT, size, valuation, and liquidity into a single dataset
total <- Reduce(
  function(x, y) merge(x, y, by = c('ts_code', 'month'), all = FALSE),
  list(AFTs_RV, cmktv_last[, -1], PE_last[, -1], Liquid)
)

# Save firm-level characteristics
write.csv(total, 'total_zz500.csv')

# ------------------------------------------------------------
# Portfolio sorting based on firm characteristics
# ------------------------------------------------------------

# Quintile sorting by PE (Value)
total <- total %>%
  group_by(month) %>%
  mutate(
    group_PE = cut(
      PE,
      quantile(PE, seq(0, 1, by = 0.2)),
      include.lowest = TRUE,
      labels = c('L', '2', '3', '4', 'H')
    )
  ) %>% ungroup()

# Quintile sorting by market capitalization (Size)
total <- total %>%
  group_by(month) %>%
  mutate(
    group_cmktv = cut(
      cmktv,
      quantile(cmktv, seq(0, 1, by = 0.2)),
      include.lowest = TRUE,
      labels = c('L', '2', '3', '4', 'H')
    )
  ) %>% ungroup()

# Quintile sorting by liquidity
total <- total %>%
  group_by(month) %>%
  mutate(
    group_Liquid = cut(
      Liquid,
      quantile(Liquid, seq(0, 1, by = 0.2)),
      include.lowest = TRUE,
      labels = c('L', '2', '3', '4', 'H')
    )
  ) %>% ungroup()

# Quintile sorting by AFT (sign reversed for interpretation)
total$AFT_RV <- -total$AFT_RV

total <- total %>%
  group_by(month) %>%
  mutate(
    group_AFT = cut(
      AFT_RV,
      quantile(AFT_RV, seq(0, 1, by = 0.2)),
      include.lowest = TRUE,
      labels = c('L', '2', '3', '4', 'H')
    )
  ) %>% ungroup()
