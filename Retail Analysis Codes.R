# Install and load necessary libraries
library(tidyverse)
library(caret)
library(lubridate)
library(dplyr)
library(corrplot)
library(knitr)
library(ggplot2)

# Data Collection
sales_data <- read.csv(file.choose())
features_data <- read.csv(file.choose())
stores_data <- read.csv(file.choose())
# 2.3 Data Processing and Pipeline

# Data Cleaning
clean_sales_data <- sales_data %>%
  filter(Weekly_Sales >= 0)# Handling negative values in Sales data frame

clean_features_data <- features_data %>%
  drop_na() %>%
  distinct()
clean_stores_data <- stores_data %>%
  drop_na() %>%
  distinct()



# Convert the Date column to a Date format in all data frames
clean_sales_data$Date <- as.Date(clean_sales_data$Date, format="%d/%m/%Y")
clean_features_data$Date <- as.Date(clean_features_data$Date, format="%d/%m/%Y")

# Merge the datasets using left joins
merged_data <- sales_data %>%
  left_join(features_data, by = c("Store", "Date")) %>%
  left_join(stores_data, by = "Store")

# Print the head of the merged data
head(merged_data)

merged_data$Holiday_Ref <- as.factor(merged_data$Holiday_Ref)

# Figure 7. Holiday Reference for Corresponding Week Numbers

# Create the Week column
merged_data$Week <- lubridate::week(merged_data$Date)

# Merge the holiday_table with the lookup table using IsHoliday as the k

# Convert the Date column to a Date format
merged_data$Date <- as.Date(merged_data$Date, format="%d/%m/%Y")

# Create the Week column
merged_data$Week <- lubridate::week(merged_data$Date)

# Create the Holiday_Ref column based on the specified holiday names
merged_data$Holiday_Ref <- ifelse(merged_data$Week == 6, "SupperBowl Week",
                                  ifelse(merged_data$Week == 36, "LaborDay Week",
                                         ifelse(merged_data$Week == 47, "ThanksGiving Week",
                                                ifelse(merged_data$Week == 52, "Christmas Week", "Non Holiday Week"))))

# Print the updated merged_data
head(merged_data)

# Filter merged_data to include only rows with holidays
holiday_week_ref <- merged_data %>%
  filter(Holiday_Ref != "Non Holiday Week") %>%
  select(Week, Holiday_Ref) %>%
  distinct()
kable(holiday_week_ref)



# Figure 8. Weekely sales vs Stores
# Weekly_Sales vs. Store
ggplot(merged_data, aes(x = Store, y = Weekly_Sales, color = Holiday_Ref)) +
  geom_point() +
  labs(title = "Weekly Sales vs. Store",
       x = "Store",
       y = "Weekly Sales") +
  scale_x_continuous(breaks = seq(0, 45, by = 1), limits = c(0, 45))

# Figure 9. Weekely sales vs Temperature
# Weekly_Sales vs. Temperature
ggplot(merged_data, aes(x = Temperature, y = Weekly_Sales, color = Holiday_Ref)) +
  geom_point() +
  labs(title = "Weekly Sales vs. Temperature",
       x = "Temperature",
       y = "Weekly Sales") +
  scale_x_continuous(breaks = seq(-3, 105, by = 2), limits = c(-3, 105))


