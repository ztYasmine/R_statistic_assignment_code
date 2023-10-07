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

data_url1 <- "https://raw.githubusercontent.com/ztYasmine/R_statistic_assignment_code/main/assignment%20data/CSCO-Monthly.csv"
data_url2 <-"https://raw.githubusercontent.com/ztYasmine/R_statistic_assignment_code/main/assignment%20data/Monthly_RFM.csv"
csc <- read.csv(data_url1)
mkt_data <- read.csv(data_url2)

#modify the file name and prepare to codify the data
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
# Merge the Cisco and market data sets by the 'Date' column
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
# Calculate the mean return of Cisco over the specified date range
mean(df$return, na.rm = TRUE)




