library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggcorrplot)

#reading 1 of 50 files just to understand the structure
x<-read_csv("Data/sales_week_starting_2012-10-01.csv")
glimpse(x)

# Using a function to get the list of CSV files in the "Data" folder
csv_files <- list.files(path = "Data", pattern = "*.csv", full.names = TRUE)

# Creating a mixed dataset
x <- csv_files %>%
  lapply(read.csv) %>%  # Read each file into a list of data frames
  bind_rows()           # Combine all data frames into one

# Setting the right format
x$sale_time <- as.POSIXct(x$sale_time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
x$sale_day<-as.Date(x$sale_time, format = "%Y-%m-%d")

# Exercise 1. Plot daily sales for all 50 weeks.
daily_sales <- x %>% 
  group_by(sale_day) %>%
  summarise(
    total_sales = n(),  # Count rows within the current group
    f_sales = sum(purchaser_gender == "female", na.rm = TRUE),  # Count female sales in the group
    m_sales = sum(purchaser_gender == "male", na.rm = TRUE),    # Count male sales in the group
    .groups = 'drop'
  )

# first explarotary plot
ggplot(daily_sales, aes(y = total_sales, x = sale_day)) +
  geom_bar(stat = "identity")+
  labs(title = "Total Sales by Day", x = "Sale Day", y = "Total Sales")+
  theme_bw()

# there's has been a spike in sales, let's see when it happened
daily_sales$delta <- daily_sales$total_sales-lag(daily_sales$total_sales)

# Exercise 2. Finding the date when it spiked
daily_sales%>%
  filter(delta == max(delta, na.rm = TRUE))

# Exercise 3. Is the change in daily sales at the date you selected statistically significant? If so, what is the p-value?
## to do so, i will compare the mean sales before and after the s