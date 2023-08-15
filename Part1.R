# Load necessary libraries
library(dplyr)   
library(tidyr)
library(DataExplorer)

# Load the dataset from a CSV file named "airBnb.csv"
data <- read.csv("airBnb.csv")

# Display the structure of the loaded dataset
str(data)

# Drop columns that we don't need
data$id <- NULL
data$host.id <- NULL
data$host.name <- NULL
data$country <- NULL
data$availability.365 <- NULL

unique_values_per_column <- lapply(data, unique)

print(unique_values_per_column)


# Replace empty values with NA
data <- data %>%
  mutate_all(~ ifelse(. == "", NA, .))


# Calculate the percentage of missing values in each column of the dataset
missing_percentage <- data %>%
  summarise_all(~ (sum(is.na(.)) / length(.)) * 100)

# print the plot shows missing percentage
plot_missing(data)

# Get the column names with missing percentages greater than 15
columns_to_drop_indices <- which(missing_percentage > 15)
columns_to_drop <- names(data)[columns_to_drop_indices]

# Drop the specified columns with high missing percentages
data <- data %>%
  select(-one_of(columns_to_drop))

# Calculate the missing values percentage for each row
missing_percentage_per_row <- data %>%
  mutate(missing_count = rowSums(is.na(.)),
         missing_percentage = (missing_count / ncol(.)) * 100) %>%
  select(-missing_count)

# Drop rows with missing_percentage >= 30
data_filtered <- missing_percentage_per_row %>%
  filter(missing_percentage < 30)

# Remove dollar and comma signs and convert columns to numeric
data_filtered$service.fee <- sub("\\$", "", data_filtered$service.fee)
data_filtered$service.fee <- sub("\\,", "", data_filtered$service.fee)
data_filtered$service.fee <- as.numeric(data_filtered$service.fee)

data_filtered$price <- sub("\\$", "", data_filtered$price)
data_filtered$price <- sub("\\,", "", data_filtered$price)
data_filtered$price <- as.numeric(data_filtered$price)


# Convert numeric values to their absolute values in all columns
data_filtered <- data_filtered %>%
  mutate_if(is.numeric, abs)

# Fill numeric missing values with the mean value of the column
data_filled <- data_filtered %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Fill logical missing values with the mode value of the column
mode_logical <- which.max(table(data_filled$instant_bookable))
data_filled$instant_bookable[is.na(data_filled$instant_bookable)] <- mode_logical

# Replace missing values with mode for each nominal column
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

data_filled <- data_filled %>%
  mutate(across(where(is.character), ~ ifelse(is.na(.), get_mode(.), .)))

# Check if there are any remaining missing values and plot it
missing_percentage <- data_filled %>%
  summarise_all(~ (sum(is.na(.)) / length(.)) * 100)

plot_missing(missing_percentage)

# Remove duplicates from filled-dataset
data_filled <- unique(data_filled)

# Check if there are any remaining duplicates
sum(duplicated(data_filled))


data_filled$missing_percentage <- NULL

# find outliers for numeric values
numeric <- data_filled[c("Construction.year", "price","service.fee")]
boxplot(numeric)

# boxplot for Construction.year
boxplot(data_filled$Construction.year, main = "Boxplot of Numeric Column")

# boxplot for price
boxplot(data_filled$price, main = "Boxplot of Numeric Column")

# boxplot for service.fee
boxplot(data_filled$service.fee, main = "Boxplot of Numeric Column")

# Write the cleaned dataset to a CSV file named "cleaned_dataset.csv"
write.csv(data_filled, "cleaned_dataset.csv", row.names = FALSE)
