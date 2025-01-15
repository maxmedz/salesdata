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
hour
# first explarotary plot
ggplot(daily_sales, aes(y = total_sales, x = sale_day)) +
  geom_bar(stat = "identity")+
  labs(title = "Total Sales by Day", x = "Sale Day", y = "Total Sales")+
  theme_bw()

# there's has been a spike in sales, let's see when it happened
daily_sales$delta <- daily_sales$total_sales-lag(daily_sales$total_sales)

# Exercise 2. Finding the date when it spiked
spike_day<-daily_sales%>%
  filter(delta == max(delta, na.rm = TRUE))%>%
  pull(sale_day)
# Exercise 3.
#Is the change in daily sales at the date you selected statistically significant? If so, what is the p-value?
## to do so, i will compare the mean sales before and after the spike

daily_sales$spiked<-
  ifelse(daily_sales$sale_day<spike_day,0,1)

t.test(total_sales ~ spiked, data = daily_sales, var.equal = TRUE)
## the difference in means is significant
## p-value < 2.2e-16


# Exercise 4. 
#Does the data suggest that the change in daily sales is due to a shift in the proportion of 
# male-vs-female customers? 
# Please use plots to support your answer (a rigorous statistical analysis is not necessary).

library(ggplot2)
glimpse(daily_sales)
library(gridExtra)
p1<-ggplot(daily_sales) +
  geom_line(aes(y = total_sales, x = sale_day))

p2<-ggplot(daily_sales) +
  geom_line(aes(y = 100 * m_sales / total_sales, x = sale_day ))
grid.arrange(p1,p2,ncol=1)

# Exercise 5.
# Assigning a time period to a sale and counting sales in a particular time period

###my way
x$sale_hour<-hour(x$sale_time)
x$daypart<-ifelse(x$sale_hour %in% c(0:5),"night",
                  ifelse(x$sale_hour %in% c(6:11),"morning",
                         ifelse(x$sale_hour %in% c(12:17),"afternoon",
                                "evening"))
)

library(dplyr)


###optimal way
x <- x %>%
  mutate(daypart = case_when(
    sale_hour %in% 0:5 ~ "night",
    sale_hour %in% 6:11 ~ "morning",
    sale_hour %in% 12:17 ~ "afternoon",
    TRUE ~ "evening"  # Default case
  ))


#now let's count the shares of dayparts
proportions<-round(prop.table(table(x$daypart))*100,2)

#let's examine gender split across dayparts

gender_sales<-x%>% 
  group_by(purchaser_gender)%>%
  summarise(
    night_sales=sum(daypart=="night"),
    morning_sales=sum(daypart=="morning"),
    afternoon_sales=sum(daypart=="afternoon"),
    evening_sales=sum(daypart=="evening"),
    total_sales=n()
  )
gender_sales
