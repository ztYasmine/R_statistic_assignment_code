# using 'close' as price stat
# --------------------Assignment 3 Part 1------------------------------

# Step 1: Load Required Packages
# Load packages for reading Excel files, data manipulation, and working with dates
install.packages('readr')
library(readxl) 
library(dplyr) 
library(lubridate) 
library(readr)

# Step 2: Import and Process Cisco Data
# -----------------read date from Cisco-------------------------

csc_month <- read.csv("assignment data/CSCO-Monthly.csv")

#modify the file name and prepare to codify the data
csc <- csc_month
sapply(csc,typeof)

#replement: 'OPEN': the price of a day when openning stock market
# USE 'open' as Data aperture
# DATA CHOICE: 'CLOSE' & 'DATE'
# Convert the 'Date' column to a date format and the 'Open' column to numeric format
csc$Date <- ymd(csc$Date)

# Calculate returns based on the opening prices 
csc <- csc %>% mutate(return = (Close - lag(Close)) / lag(Close))
csc <- na.omit(csc)
# Modify The table into dataset I need

csc_cal <- csc[,c('Date','Close','return')]

# ---------------------------------------------
# Step 3: Import and Process Market Data
# ---------------------------------------------
# Load market data from an Excel file
mkt_data <- read.csv("assignment data/Monthly_RFM.csv")
colnames(mkt_data)<-c('Date','Mkt_rf',"RF")
# Process the market data to convert relevant columns to appropriate formats and units
mkt_data <- mkt_data %>% mutate(
         Mkt_rf = as.numeric(Mkt_rf)/100, 
         RF = as.numeric(RF)/100)

# uniform the date version
csc_cal$Date <- format(csc_cal$Date, "%Y-%m")
mkt_data$Date <- paste(substr(mkt_data$Date,1,4), substr(mkt_data$Date,5,6), sep="-")

# --------------------------------
# Step 4: Merge Data Sets
# --------------------------------
# Merge the Walmart and market data sets by the 'Date' column
df = merge(csc_cal, mkt_data, by = "Date")

# Calculate the Cisco excess return 
df = df %>% mutate(excess_return = return - RF)

# ------------------------------
# Step 5: Run Linear Regression
# ------------------------------
# remove the INF
df <- df[!is.infinite(df$return),]
ols <- lm(excess_return ~ Mkt_rf, data = df)
print(summary(ols)) 


# ------------------------------
# Step 6: Additional Analysis
# ------------------------------
# Calculate the mean return of Walmart over the specified date range
mean(df$return, na.rm = TRUE)



