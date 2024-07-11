# Load necessary libraries
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(dbscan)

# Set working directory and data directory
WD <- getwd()
DIR_DATA <- file.path(WD, "Data")

# Read energy data file
file_path <- file.path(DIR_DATA, "historical_DIGIPEN_from_20240101_updated.csv")
data <- read.csv(file_path)

# Convert Date column to POSIXct format with the correct format
data$Date <- as.POSIXct(data$Date, format = "%d-%m-%Y;%H:%M:%S.%OS")

# Remove all values below zero in column EE_BC2_TER
data <- data %>% mutate(EE_BC2_TER = ifelse(EE_BC2_TER < 0, NA, EE_BC2_TER))

# Define the columns to be converted
columns_to_convert <- c("ET_BC1_Frio", "ET_BC1_Calor", "ET_BC2_Frio", "ET_BC2_Calor", "ET_BC3_Frio", "ET_BC3_Calor")

# Multiply the specified columns by 1000 to convert MWh to kWh
data <- data %>%
  mutate(across(all_of(columns_to_convert), ~ . * 1000))

# Variables of interest
vars_of_interest <- c("EE_BC1", "ET_BC1_Frio", "ET_BC1_Calor",
                      "EE_BC2", "ET_BC2_Frio", "ET_BC2_Calor",
                      "EE_BC3", "ET_BC3_Frio", "ET_BC3_Calor",
                      "EE_BC1_TER", "EE_BC2_TER",
                      "T_SAL_BC1", "T_ENT_BC1",
                      "T_SAL_BC2", "T_ENT_BC2",
                      "T_SAL_BC3", "T_ENT_BC3")

# Remove zero values from specified columns
data[vars_of_interest] <- lapply(data[vars_of_interest], function(x) ifelse(x == 0, NA, x))

# Calculate differences and store in new columns with _actual suffix
for (var in vars_of_interest) {
  data[[paste0(var, "_actual")]] <- c(NA, diff(data[[var]]))
}

# Subtract T_SAL_BC1 from T_ENT_BC1 and store in Delta 1
data <- data %>% mutate(Delta1 =  T_SAL_BC1-T_ENT_BC1)

# Subtract T_SAL_BC2 from T_ENT_BC2 and store in Delta 2
data <- data %>% mutate(Delta2 =  T_SAL_BC2-T_ENT_BC2)

# Subtract T_SAL_BC3 from T_ENT_BC3 and store in Delta 3
data <- data %>% mutate(Delta3 =  T_SAL_BC3-T_ENT_BC3 )

# Replace NA with 0 in columns ending with _actual
actual_cols <- grep("_actual$", names(data), value = TRUE)
data[actual_cols][is.na(data[actual_cols])] <- 0

# Define a function to remove DBSCAN outliers
remove_dbscan_outliers <- function(x, eps = 0.5, minPts = 5) {
  x <- na.omit(x)  # Remove NAs
  db <- dbscan::dbscan(as.matrix(x), eps = eps, minPts = minPts)
  # Mark outliers (cluster 0) as NA
  x[db$cluster == 0] <- NA
  return(x)
}

# Apply the outlier removal function to specified columns
data <- data %>%
  mutate(
    EE_BC1_TER_actual = remove_dbscan_outliers(EE_BC1_TER_actual),
    EE_BC2_TER_actual = remove_dbscan_outliers(EE_BC2_TER_actual)
  )

# Plot each _actual column against Date
plots <- lapply(actual_cols, function(col) {
  ggplot(data, aes(x = Date, y = .data[[col]])) +
    geom_point() +
    labs(title = col, x = "Date", y = "kWh")
})

# Display all plots
for (plot in plots) {
  print(plot)
}

# Save the pre-processed data as CSV in DIR_DATA
write.csv(data, file = file.path(DIR_DATA, "pre_processed.csv"), row.names = FALSE)
