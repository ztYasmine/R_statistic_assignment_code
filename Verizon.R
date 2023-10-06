library(readxl) 
library(dplyr) 
library(lubridate) 



# ------------------------------------------
# Set the working directory 
setwd("~/Desktop/GBA 463")

# Load Verizon price data from a CSV file
VZ = read_csv("VZ.csv")


# Select only the 'Date' and 'Open' columns for further analysis
VZ = VZ %>% select(Date, Open)

# Convert the 'Date' column to a date format and the 'Open' column to numeric format
VZ$Date = ymd(VZ$Date)
VZ$Open = as.numeric(VZ$Open)

# Calculate returns based on the opening prices 
VZ = VZ %>% mutate(return = (Open - lag(Open)) / lag(Open))
VZ = na.omit(VZ)


# ---------------------------------------------
# Load market data from an csv file
mkt_data = read_csv("F-F_Research_Data_Factors.csv")

# Process the market data to convert relevant columns to appropriate formats and units
mkt_data <- mkt_data %>% 
  mutate(Date = ym(Date), # facilitate merging
         mkt_rf = as.numeric(mkt_rf) / 100, # Convert to fraction
         rf = as.numeric(rf) / 100) # Convert to fraction



# --------------------------------
# Merge the Verizon and market data sets by the 'Date' column
df = merge(VZ, mkt_data, by = "Date")

# Filter the merged data
df = df %>% filter(Date >= "1990-01-01" & Date <= "1999-12-01")

# Calculate the Verizon excess return 
df = df %>% mutate(excess_return = return - rf)

# ------------------------------
# Run Linear Regression
ols = lm(excess_return ~ mkt_rf, data = df)
print(summary(ols)) 


ER_VZ <- mean(df$return, na.rm = TRUE)

# 1) I chose the open price of VZ's stake because 






