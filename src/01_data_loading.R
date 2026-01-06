# ============================================================
# Module 1: Data Loading and Stock Universe Construction
# ============================================================
# This module constructs the stock universe and loads
# daily stock return data as well as index constituents.
# The universe is restricted based on listing date
# and CSI 500 index membership to ensure consistency
# in the empirical analysis.
# ============================================================

setwd("D:\\文档\\work_data_2022_12")
# Set working directory for data access

require(dplyr)
require(reshape)
require(reshape2)
require(magrittr)
require(data.table)

# ------------------------------------------------------------
# Load stock universe information
# ------------------------------------------------------------

# Load firm-level listing information
# Only firms listed before January 1, 2003 are retained
# to avoid survivorship and listing bias
st <- read.csv('公司文件003241203\\TRD_Co.csv')

stock_list <- st[which(st$Listdt <= '2003-01-01'), ]$Stkcd

# ------------------------------------------------------------
# Load CSI 500 index constituents
# ------------------------------------------------------------

# Import CSI 500 index constituent file
# The universe is further restricted to stocks
# included in the CSI 500 index
zz500 <- read.csv('指数成分股权重文件173742212\\IDX_Smprat.csv')

stock_list <- unique(zz500$Enddt)

# ------------------------------------------------------------
# Load daily stock return files
# ------------------------------------------------------------

# Identify folders containing daily stock return data
dirs <- list.files()
path_list <- subset(dirs, grepl('日个股回报率文件', dirs))

# Define a function to import and concatenate all CSV files
# from the specified directories
import_csv_files <- function(path_list) {
  
  # Initialize an empty data frame to store combined data
  df <- data.frame()
  
  # Loop over each directory
  for (dir in path_list) {
    
    filenames <- list.files(dir)
    
    # Retain only CSV files and exclude text files
    filenames <- subset(
      filenames,
      grepl('.csv', filenames) & !grepl('.txt', filenames)
    )
    
    cat('[INFO]', as.character(Sys.time()),
        'Start reading CSV files in directory:', dir, '\n')
    
    # Import each CSV file and append to the data frame
    for (file in filenames) {
      data <- read.csv(paste(dir, file, sep = "\\"))
      df <- rbind(df, data)
      
      cat('[INFO]', as.character(Sys.time()),
          'File loaded:', paste(dir, file, sep = "\\"), '\n')
    }
    
    cat('\n')
  }
  
  cat('[INFO]', as.character(Sys.time()),
      'All daily stock return data successfully loaded\n')
  
  return(df)
}

# Load and concatenate all daily return data
df <- import_csv_files(path_list)

# Filter stocks based on the predefined universe
data <- df[which(df$Stkcd %in% stock_list), ]

# Save filtered daily return data
write.csv(data, file = 'data_list_zz500.csv')

# ------------------------------------------------------------
# Reshape daily returns into wide format
# ------------------------------------------------------------

# Convert daily returns to a wide matrix format
# Rows represent trading dates, columns represent stocks
ret <- reshape2::dcast(
  data,
  Trddt ~ Stkcd,
  value.var = "ChangeRatio"
)

View(ret)
