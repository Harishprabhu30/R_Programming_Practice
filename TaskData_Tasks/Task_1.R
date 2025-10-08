library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)


data <- read_tsv("TaskData_Tasks/taskdata.tsv")
View(data)

# finding the number of missing values in each column.
colSums((is.na(data)))

# Errors check
# glimpse(data)
summary(data)

# outliers check
find_outliers <- function(x) {
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.5 * iqr
    upper_bound <- q1 + 1.5 * iqr
    x[x < lower_bound | x > upper_bound]
}

# apply the function to numeric columns
outliers_table <- data %>%
    select(where(is.numeric)) %>%
    summarise_all(~list(find_outliers(.)))

outliers_table
# The output is a tibble where each cell contains a list of outliers for that column

# A. Scatter plot between age and income with all data
ggplot(data, aes(x = age, y = income)) + geom_point() + ggtitle("Scatter plot between age and income")

# B. Filter data: age (0-100), income > 0
filtered_data <- data %>% 
filter(age >= 0 & age <= 100, income > 0)

ggplot(filtered_data, aes(x = age, y = income)) + geom_point() + ggtitle("Scatter plot between age and income (Filtered Data)")

# C. Highlight customers with income > 100000
ggplot(filtered_data, aes(x = age, y = income, color = income > 100000)) + geom_point() + 
scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"), name = "High Income (>100K)") +
ggtitle("Scatter plot between age and income (Highlighted High Income")