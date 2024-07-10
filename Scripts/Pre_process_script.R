library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)

# Set working directory and data directory
WD <- getwd()
DIR_DATA <- file.path(WD, "Data")

# Read energy data file
file_path <- file.path(DIR_DATA, "historical_DIGIPEN_from_20240101_updated.csv")
data <- read.csv(file_path)

# Convert Date column to POSIXct format with the correct format
data$Date <- as.POSIXct(data$Date, format = "%d-%m-%Y;%H:%M:%S.%OS")

# Define the columns to be converted
columns_to_convert <- c("ET_BC1_Frio", "ET_BC1_Calor", "ET_BC2_Frio", "ET_BC2_Calor", "ET_BC3_Frio", "ET_BC3_Calor")

# Multiply the specified columns by 1000 to convert MWh to kWh
data <- data %>%
  mutate(across(all_of(columns_to_convert), ~ . * 1000))

# Variables of interest
vars_of_interest <- c("EE_BC1", "ET_BC1_Frio", "ET_BC1_Calor",
                      "EE_BC2", "ET_BC2_Frio", "ET_BC2_Calor",
                      "EE_BC3", "ET_BC3_Frio", "ET_BC3_Calor",
                      "EE_BC1_TER","EE_BC2_TER")

# Remove zero values from specified columns
data[vars_of_interest] <- lapply(data[vars_of_interest], function(x) ifelse(x == 0, NA, x))

# Calculate differences and store in new columns with _actual suffix
for (var in vars_of_interest) {
  data[[paste0(var, "_actual")]] <- c(NA, diff(data[[var]]))
}

# Replace NA with 0 in columns ending with _actual
actual_cols <- grep("_actual$", names(data), value = TRUE)
data[actual_cols][is.na(data[actual_cols])] <- 0

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
