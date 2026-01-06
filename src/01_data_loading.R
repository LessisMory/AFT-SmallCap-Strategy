setwd("D:\\文档\\work_data_2022_12")
# Set working directory for raw data storage

require(dplyr)
require(reshape)
require(reshape2)
require(magrittr)
require(data.table)

# ============================================================
# Load stock universe
# ============================================================

# Load list of firms listed before 2003
# Source: CSMAR company information file (TRD_Co.csv)
st <- read.csv('公司文件003241203\\TRD_Co.csv')
stock_list = st[which(st$Listdt <= '2003-01-01'), ]$Stkcd

# ============================================================
# Load daily stock return data
# ============================================================

# Identify folders containing daily stock return files
dirs <- list.files()
path_list <- subset(dirs, grepl('日个股回报率文件', dirs))

# Helper function: import and concatenate CSV files from multiple folders
import_csv_files <- function(path_list) {
  df <- data.frame()
  for (dir in path_list) {
    filenames <- list.files(dir)
    filenames <- subset(filenames, grepl('.csv', filenames) & !grepl('.txt', filenames))
    cat('[INFO]', as.character(Sys.time()), 'Start reading CSV files from folder:', dir, '\n')
    for (file in filenames) {
      data <- read.csv(paste(dir, file, sep = "\\"))
      df <- rbind(df, data)
      cat('[INFO]', as.character(Sys.time()), 'Loaded file:', paste(dir, file, sep = "\\"), '\n')
    }
    cat('\n')
  }
  cat('[INFO]', as.character(Sys.time()), 'All daily return data successfully imported\n')
  return(df)
}

# Import daily return data
df <- import_csv_files(path_list)

# Filter stocks within the predefined universe
data = df[which(df$Stkcd %in% stock_list), ]

# Save filtered return data
write.csv(data, file = 'data_list_zz500.csv')

# Reshape daily returns into wide format (date × stock)
ret = reshape2::dcast(data, Trddt ~ Stkcd, value.var = "ChangeRatio")

# ============================================================
# Load realized volatility (RV) data
# ============================================================

setwd("D:\\文档\\work_data_2022_12\\new")

# Identify folders containing realized volatility files
dirs <- list.files()
path_list <- subset(dirs, grepl('已实现', dirs))

# Import realized volatility data
df <- import_csv_files(path_list)

# Filter stocks within the predefined universe
data <- df[which(df$Stkcd %in% stock_list), ]

# Reshape RV data into wide format
RV <- reshape2::dcast(data, Trddt ~ Stkcd, value.var = "RV")

# Convert RV data to long format
RV <- reshape2::melt(
  RV,
  id.vars = 'Trddt',
  variable.name = 'ts_code',
  value.name = 'RV'
)

colnames(RV) <- c("trade_date", "ts_code", "RV")

# Convert stock codes to integer format
RV$ts_code <- RV$ts_code %>%
  sapply(function(x) x %>% as.character() %>% as.integer())

# Replace missing RV values with zeros
setnafill(RV, "const", cols = c('RV'), fill = 0)

# Extract month identifier for later aggregation
RV$month <- RV$trade_date %>%
  sapply(function(x) substr(x, 1, 7))

RV <- data.table(RV)
