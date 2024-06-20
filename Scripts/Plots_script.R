library(tidyr)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(lubridate)
library(magrittr)

# Set working directory and data directory
WD <- getwd()
DIR_DATA <- file.path(WD, "Data")
DIR_WEATHER <- file.path(WD, "Weather files")

# Set the directory for saving plots
plot_dir <- file.path(WD, "Plots")

# List all CSV files in the data directory
file_list <- list.files(DIR_DATA, pattern = "\\.csv$", full.names = TRUE)
if (length(file_list) == 0) {
  stop("No files found in the data directory.")
}

# Read the specified CSV file
data_file <- file.path(DIR_DATA, "pre_processed.csv")
if (!file.exists(data_file)) {
  stop("csv file not found in the data directory.")
}
data <- read.csv(data_file)

# Read weather data file
weather_file_list <- list.files(DIR_WEATHER, full.names = TRUE)
if (length(weather_file_list) == 0) {
  stop("No files found in the weather directory.")
} else {
  weather_file_path <- weather_file_list[1]
  weather_data <- read.csv(weather_file_path)
}

# Convert 'Date' column to POSIXct object to include both date and time
data$Date <- as.POSIXct(data$Date, format = "%Y-%m-%d %H:%M", tz = "UTC")

# Extract hour from Date column
data$Hour <- hour(data$Date)

# Find maximum y-axis values for each category
max_EE <- max(data$EE_BC1_actual, data$EE_BC2_actual, na.rm = TRUE)
max_ET_Frio <- max(data$ET_BC1_Frio_actual, data$ET_BC2_Frio_actual, na.rm = TRUE)
max_ET_Calor <- max(data$ET_BC1_Calor_actual, data$ET_BC2_Calor_actual, na.rm = TRUE)

# Set the y-axis limits for each category
y_limits <- list(
  EE = c(0, max_EE),
  ET_Frio = c(0, max_ET_Frio),
  ET_Calor = c(0, max_ET_Calor)
)

# Function to plot and save images for a specific heat pump
plot_and_save_image <- function(data, prefix, plot_dir, y_limits) {
  
  # Plot EE_BC vs Date
  ee_plot <- ggplot(data, aes(x = Date, y = !!sym(paste0("EE_BC", prefix, "_actual")))) +
    geom_point() +
    ylim(y_limits$EE) +
    scale_x_datetime(date_labels = "%b %Y", date_breaks = "1 month") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Date", y = paste("EE_BC", prefix, "(kWh)"), title = paste("EE_BC", prefix, "vs Date"))
  
  # Save the plot as a PNG file
  ggsave(file.path(plot_dir, paste("EE_BC", prefix, "_vs_Date.png")), ee_plot)
  print(ee_plot)
  
  # Plot ET_BC_Frio vs Date
  et_frio_plot <- ggplot(data, aes(x = Date, y = !!sym(paste0("ET_BC", prefix, "_Frio_actual")))) +
    geom_point() +
    ylim(y_limits$ET_Frio) +
    scale_x_datetime(date_labels = "%b %Y", date_breaks = "1 month") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Date", y = paste("ET_BC", prefix, "_Frio (kWh)"), title = paste("ET_BC", prefix, "_Frio vs Date"))
  
  # Save the plot as a PNG file
  ggsave(file.path(plot_dir, paste("ET_BC", prefix, "_Frio_vs_Date.png")), et_frio_plot)
  print(et_frio_plot)
  
  # Plot ET_BC_Calor vs Date
  et_calor_plot <- ggplot(data, aes(x = Date, y = !!sym(paste0("ET_BC", prefix, "_Calor_actual")))) +
    geom_point() +
    ylim(y_limits$ET_Calor) +
    scale_x_datetime(date_labels = "%b %Y", date_breaks = "1 month") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Date", y = paste("ET_BC", prefix, "_Calor (kWh)"), title = paste("ET_BC", prefix, "_Calor vs Date"))
  
  # Save the plot as a PNG file
  ggsave(file.path(plot_dir, paste("ET_BC", prefix, "_Calor_vs_Date.png")), et_calor_plot)
  print(et_calor_plot)
}

# Plot and save images for each heat pump
for (i in 1:3) {
  plot_and_save_image(data, i, plot_dir, y_limits)
}

# Making hourly plots of all heat pumps against EE, ET_Frio, ET_Calor
# Filter out rows with NA in Date column
data <- data[!is.na(data$Date), ]

# Define a vector of prefixes
prefixes <- c("BC1", "BC2", "BC3")

# Extract hour from Date column
data$Hour <- as.numeric(format(data$Date, "%H"))

# Determine weekday or weekend
data$DayType <- ifelse(weekdays(data$Date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

# Create an empty data frame to store hourly data for all heat pumps
hourly_data_combined <- data.frame()

# Iterate over each prefix to calculate hourly values
for (prefix in prefixes) {
  # Aggregate data by Hour, DayType, and Heat_Pump
  hourly_data <- data %>%
    group_by(Hour, DayType) %>%
    summarize(
      EE_hourly = mean(!!sym(paste0("EE_", prefix, "_actual")), na.rm = TRUE),
      ET_Frio_hourly = mean(!!sym(paste0("ET_", prefix, "_Frio_actual")), na.rm = TRUE),
      ET_Calor_hourly = mean(!!sym(paste0("ET_", prefix, "_Calor_actual")), na.rm = TRUE)
    ) %>%
    mutate(Heat_Pump = prefix)
  
  # Combine hourly data
  hourly_data_combined <- bind_rows(hourly_data_combined, hourly_data)
}

# Filter out extreme values (outliers) for better visualization
hourly_data_combined <- hourly_data_combined %>%
  filter(ET_Calor_hourly < 30 & ET_Calor_hourly > -10)  # Adjust the threshold as needed

# Find maximum y-axis values for each category
max_EE <- max(hourly_data_combined$EE_hourly, na.rm = TRUE)
max_ET_Frio <- max(hourly_data_combined$ET_Frio_hourly, na.rm = TRUE)
max_ET_Calor <- max(hourly_data_combined$ET_Calor_hourly, na.rm = TRUE)

# Set the y-axis limits for each category
y_limits <- list(
  EE = c(0, max_EE),
  ET_Frio = c(0, max_ET_Frio),
  ET_Calor = c(0, max_ET_Calor)
)

# Plot EE_actual vs Hour with line plot for all prefixes, distinguishing by DayType
plot_EE <- ggplot(hourly_data_combined, aes(x = Hour, y = EE_hourly, color = Heat_Pump, group = Heat_Pump)) +
  geom_line(size = 1.5) +  # Increase line thickness
  facet_wrap(~DayType) +  # Separate plots by DayType (Weekday vs Weekend)
  ylim(y_limits$EE) +
  labs(x = "Hour", y = "EE (kwh)", title = "Hourly EE by Weekday/Weekend for BC1, BC2, and BC3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 10))  # Set the font size of the plot title

# Plot ET_Frio_actual vs Hour with line plot for all prefixes, distinguishing by DayType
plot_ET_Frio <- ggplot(hourly_data_combined, aes(x = Hour, y = ET_Frio_hourly, color = Heat_Pump, group = Heat_Pump)) +
  geom_line(size = 1.5) +  # Increase line thickness
  facet_wrap(~DayType) +  # Separate plots by DayType (Weekday vs Weekend)
  ylim(y_limits$ET_Frio) +
  labs(x = "Hour", y = "ET_Frio (kwh)", title = "Hourly ET_Frio by Weekday/Weekend for BC1, BC2, and BC3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 10))  # Set the font size of the plot title

# Plot ET_Calor_actual vs Hour with line plot for all prefixes, distinguishing by DayType
plot_ET_Calor <- ggplot(hourly_data_combined, aes(x = Hour, y = ET_Calor_hourly, color = Heat_Pump, group = Heat_Pump)) +
  geom_line(size = 1.5) +  # Increase line thickness
  facet_wrap(~DayType) +  # Separate plots by DayType (Weekday vs Weekend)
  ylim(y_limits$ET_Calor) +
  labs(x = "Hour", y = "ET_Calor (kwh)", title = "Hourly ET_Calor by Weekday/Weekend for BC1, BC2, and BC3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 10))  # Set the font size of the plot title

# Save and display the combined plots
ggsave(file.path(plot_dir, "EE_hourly_combined_plot.png"), plot = plot_EE)
print(plot_EE)  # Display the EE plot

ggsave(file.path(plot_dir, "ET_Frio_hourly_combined_plot.png"), plot = plot_ET_Frio)
print(plot_ET_Frio)  # Display the ET_Frio plot

ggsave(file.path(plot_dir, "ET_Calor_hourly_combined_plot.png"), plot = plot_ET_Calor)
print(plot_ET_Calor)  # Display the ET_Calor plot

# Plots for day of the week
# Filter out rows with NA in Date column
data <- data[!is.na(data$Date), ]

# Define a vector of prefixes
prefixes <- c("BC1", "BC2", "BC3")

# Create a data frame to store daily data for all heat pumps
daily_data_combined <- data.frame()

# Extract day of the week from Date column
data$DayOfWeek <- wday(data$Date, label = TRUE)

# Iterate over each prefix to calculate daily values
for (prefix in prefixes) {
  # Aggregate data by day of the week
  daily_data <- data %>%
    group_by(DayOfWeek) %>%
    summarize(
      EE_daily = mean(!!sym(paste0("EE_", prefix, "_actual")), na.rm = TRUE),
      ET_Frio_daily = mean(!!sym(paste0("ET_", prefix, "_Frio_actual")), na.rm = TRUE),
      ET_Calor_daily = mean(!!sym(paste0("ET_", prefix, "_Calor_actual")), na.rm = TRUE)
    ) %>%
    mutate(Heat_Pump = prefix)
  
  # Combine daily data
  daily_data_combined <- bind_rows(daily_data_combined, daily_data)
}

# Sort by DayOfWeek to ensure correct line connections
daily_data_combined <- daily_data_combined[order(match(daily_data_combined$DayOfWeek, c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))), ]

# Find maximum y-axis values for each category
max_EE <- max(daily_data_combined$EE_daily, na.rm = TRUE)
max_ET_Frio <- max(daily_data_combined$ET_Frio_daily, na.rm = TRUE)
max_ET_Calor <- max(daily_data_combined$ET_Calor_daily, na.rm = TRUE)

# Set the y-axis limits for each category
y_limits <- list(
  EE = c(0, max_EE),
  ET_Frio = c(0, max_ET_Frio),
  ET_Calor = c(0, max_ET_Calor)
)

# Plot EE_actual vs Day of the week with line plot for all prefixes
plot_EE <- ggplot(daily_data_combined, aes(x = DayOfWeek, y = EE_daily, color = Heat_Pump, group = Heat_Pump)) +
  geom_line(size = 1.5) +  # Increase line thickness
  ylim(y_limits$EE) +
  labs(x = "Day of the Week", y = "EE (kwh)", title = "Daily EE for BC1, BC2, and BC3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 10))  # Set the font size of the plot title

# Plot ET_Frio_actual vs Day of the week with line plot for all prefixes
plot_ET_Frio <- ggplot(daily_data_combined, aes(x = DayOfWeek, y = ET_Frio_daily, color = Heat_Pump, group = Heat_Pump)) +
  geom_line(size = 1.5) +  # Increase line thickness
  ylim(y_limits$ET_Frio) +
  labs(x = "Day of the Week", y = "ET_Frio (kwh)", title = "Daily ET_Frio for BC1, BC2, and BC3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 10))  # Set the font size of the plot title

# Plot ET_Calor_actual vs Day of the week with line plot for all prefixes
plot_ET_Calor <- ggplot(daily_data_combined, aes(x = DayOfWeek, y = ET_Calor_daily, color = Heat_Pump, group = Heat_Pump)) +
  geom_line(size = 1.5) +  # Increase line thickness
  ylim(y_limits$ET_Calor) +
  labs(x = "Day of the Week", y = "ET_Calor (kwh)", title = "Daily ET_Calor for BC1, BC2, and BC3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 10))  # Set the font size of the plot title

# Save and display the combined plots
ggsave(file.path(plot_dir, "EE_combined_plot.png"), plot = plot_EE)
print(plot_EE)  # Display the EE plot

ggsave(file.path(plot_dir, "ET_Frio_combined_plot.png"), plot = plot_ET_Frio)
print(plot_ET_Frio)  # Display the ET_Frio plot

ggsave(file.path(plot_dir, "ET_Calor_combined_plot.png"), plot = plot_ET_Calor)
print(plot_ET_Calor)  # Display the ET_Calor plot

#COPs calculation
#HEAT PUMP 1

# Calculate COP for BC1
data$COP_BC1 <- (data$ET_BC1_Frio_actual + data$ET_BC1_Calor_actual) / data$EE_BC1_actual

#HEAT PUMP 2

# Calculate COP for BC2
data$COP_BC2 <- (data$ET_BC2_Frio_actual + data$ET_BC2_Calor_actual) / data$EE_BC2_actual

# Replace 'inf' values with the median value of COP_BC1
median_cop_bc1 <- median(data$COP_BC1, na.rm = TRUE)
data$COP_BC1[is.infinite(data$COP_BC1)] <- median_cop_bc1

# Replace 'inf' values with the median value of COP_BC2
median_cop_bc2 <- median(data$COP_BC2, na.rm = TRUE)
data$COP_BC2[is.infinite(data$COP_BC2)] <- median_cop_bc2

#HEAT PUMP 3

data$COP_BC3 <- (data$ET_BC3_Frio_actual + data$ET_BC3_Calor_actual) / data$EE_BC3_actual

# Calculate IQR and identify outliers for each COP column
cols_to_check <- c("COP_BC1", "COP_BC2", "COP_BC3")
for (col in cols_to_check) {
  q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
  q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_limit <- q1 - 1.5 * iqr
  upper_limit <- q3 + 1.5 * iqr
  
  # Identify and replace outliers with median
  outliers <- which(data[[col]] < lower_limit | data[[col]] > upper_limit)
  if (length(outliers) > 0) {
    median_cop <- median(data[[col]], na.rm = TRUE)
    data[[col]][outliers] <- median_cop  # Replace outliers with median
  }
}

# PLOTS COP vs Date
# Function to plot and save COP vs Date with uniform y-axis scale
plot_and_save_cop <- function(data, col_name, plot_dir, y_limits) {
  plot <- ggplot(data, aes(x = Date, y = !!sym(col_name))) +
    geom_point() +
    ylim(y_limits) +  # Set uniform y-axis limits
    labs(x = "Date", y = col_name, title = paste(col_name, "vs Date"))
  
  ggsave(file.path(plot_dir, paste(col_name, "_vs_Date.png")), plot)
  print(plot)
}

# Determine the minimum and maximum values for the y-axis
y_min <- min(c(min(data$COP_BC1, na.rm = TRUE), min(data$COP_BC2, na.rm = TRUE)))
y_max <- max(c(max(data$COP_BC1, na.rm = TRUE), max(data$COP_BC2, na.rm = TRUE)))
y_limits <- c(y_min, y_max)

# Convert Date to the desired format
data$Date_formatted <- format(data$Date, "%d-%m-%Y")

# Plot and save COP for each column with uniform y-axis scale
cols_to_plot <- c("COP_BC1", "COP_BC2", "COP_BC3")
for (col in cols_to_plot) {
  plot_and_save_cop(data, col, plot_dir, y_limits)
}

# Function to plot and save COP vs hour with different prefixes
plot_and_save_hourly_cop_combined <- function(data, prefixes, variable_name, plot_title, plot_dir) {
  # Extract hour from Date column
  data$Hour <- as.numeric(format(data$Date, "%H"))
  
  # Create an empty data frame to store combined hourly COP data
  hourly_cop_combined <- data.frame()
  
  # Iterate over each prefix
  for (prefix in prefixes) {
    # Aggregate data by hour and prefix
    hourly_data <- data %>%
      group_by(Hour) %>%
      summarize(COP_hourly = mean(!!sym(paste0(variable_name, "_", prefix)), na.rm = TRUE)) %>%
      mutate(Heat_Pump = prefix)
    
    # Combine hourly data
    hourly_cop_combined <- bind_rows(hourly_cop_combined, hourly_data)
  }
  
  # Plot hourly data for COP and store it in a variable
  plot <- ggplot(hourly_cop_combined, aes(x = Hour, y = COP_hourly, color = Heat_Pump, group = Heat_Pump)) +
    geom_line(size = 1.5) +  # Adjust line thickness
    labs(x = "Hour", y = plot_title, title = paste("Hourly", plot_title)) +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white")) +
    ylim(0, NA) +  # Adjust y-axis limits based on data
    scale_color_manual(values = c("BC1" = "blue", "BC2" = "red", "BC3" = "green"))  # Define colors for each prefix
  
  # Print the plot
  print(plot)
  
  # Save the plot as a PNG file
  ggsave(file.path(plot_dir, paste(plot_title, "_hourly_combined.png")), plot)
  
  # Return the combined hourly COP data frame
  return(hourly_cop_combined)
}

# Define prefixes
prefixes <- c("BC1", "BC2", "BC3")

# Call the function to plot and save combined hourly COP data
hourly_cop_combined <- plot_and_save_hourly_cop_combined(data, prefixes, "COP", "Combined COP", plot_dir)

# Function to plot and save COP vs day of the week with different prefixes
plot_and_save_daily_cop_combined <- function(data, prefixes, variable_name, plot_title, plot_dir) {
  # Extract day of the week from Date column
  data$DayOfWeek <- factor(weekdays(data$Date), levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
  
  # Create an empty data frame to store combined daily COP data
  daily_cop_combined <- data.frame()
  
  # Iterate over each prefix
  for (prefix in prefixes) {
    # Aggregate data by day of the week and prefix
    daily_data <- data %>%
      group_by(DayOfWeek) %>%
      summarize(COP_daily = mean(!!sym(paste0(variable_name, "_", prefix)), na.rm = TRUE)) %>%
      mutate(Heat_Pump = prefix)
    
    # Combine daily data
    daily_cop_combined <- bind_rows(daily_cop_combined, daily_data)
  }
  
  # Sort by DayOfWeek to ensure correct line connections
  daily_cop_combined <- daily_cop_combined %>%
    arrange(DayOfWeek)
  
  # Plot daily data for COP and store it in a variable
  plot <- ggplot(daily_cop_combined, aes(x = DayOfWeek, y = COP_daily, color = Heat_Pump, group = Heat_Pump)) +
    geom_line(size = 1.5) +  # Increase line thickness
    labs(x = "Day of the Week", y = plot_title, title = paste("Daily", plot_title)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
    ylim(0, NA) +  # Adjust y-axis limits based on data
    scale_color_manual(values = c("BC1" = "blue", "BC2" = "red", "BC3" = "green"))  # Define colors for each prefix
  
  # Print the plot
  print(plot)
  
  # Save the plot as a PNG file
  ggsave(file.path(plot_dir, paste(plot_title, "_daily_combined.png")), plot)
  
  # Return the combined daily COP data frame
  return(daily_cop_combined)
}

# Define prefixes
prefixes <- c("BC1", "BC2", "BC3")

# Call the function to plot and save combined daily COP data
daily_cop_combined <- plot_and_save_daily_cop_combined(data, prefixes, "COP", "Combined COP", plot_dir)

#comparison of heat pumps hourly energy consumption
# Reshape data into longer format for hourly comparison
hourly_data_long <- data %>%
  select(Hour, EE_BC1_actual, EE_BC2_actual) %>%  # Select Hour and the columns to compare
  pivot_longer(cols = -Hour, names_to = "HeatPump", values_to = "Energy_Consumption")

# Plot hourly data for EE_BC1 and EE_BC2
hourly_plot_comparison <- ggplot(hourly_data_long, aes(x = Hour, y = Energy_Consumption, color = HeatPump)) +
  geom_point() +
  labs(x = "Hour", y = "Energy Consumption(kwh)", title = "Hourly Energy Consumption by Heat Pumps comparision") +
  scale_color_manual(values = c("blue", "red")) +  # Set colors for each heat pump
  theme_minimal()

# Save the plot as a PNG file
ggsave(file.path(plot_dir, "hourly_E_Consumption_comparision.png"), hourly_plot_comparison)

print(hourly_plot_comparison)

#comparison of heat pumps daily energy consumption

# Reshape data into longer format
data_long <- data %>%
  select(Date, EE_BC1_actual, EE_BC2_actual) %>%  # Remove EE_BC3_actual from the selection
  pivot_longer(cols = -Date, names_to = "HeatPump", values_to = "Energy_Consumption")

# Plot EE_BC1_actual and EE_BC2_actual against Date with different colors
daily_plot_comparison <- ggplot(data_long, aes(x = Date, y = Energy_Consumption, color = HeatPump)) +
  geom_point() +
  labs(x = "Date", y = "Energy Consumption(kwh)", title = "Daily Energy Consumption by Heat Pumps comparision") +
  scale_color_manual(values = c("red", "blue")) +  # Set colors for each heat pump
  theme_minimal()

# Save the plot as a PNG file
ggsave(file.path(plot_dir, "daily_E_Consumption_comparision.png"), daily_plot_comparison)

print(daily_plot_comparison)

# Energy signature analysis
# Convert Dia column to Date format 
weather_data$Dia <- as.Date(weather_data$Dia)

# Aggregate the daily average temperature
daily_avg_temp <- weather_data %>%
  group_by(Dia) %>%
  summarize(Avg_Temperature = mean(Tem.Aire._a_110cm, na.rm = TRUE))

# Base temperature for HDD and CDD (in Celsius)
base_temperature <- 18.3  # 65°F

# Calculate HDD and CDD
daily_avg_temp$HDD <- pmax(base_temperature - daily_avg_temp$Avg_Temperature, 0)
daily_avg_temp$CDD <- pmax(daily_avg_temp$Avg_Temperature - base_temperature, 0)

# This code is only added until updated data can be received from EUSKALMET
################################################################################
# Define the file name for the 'HDD_CDD Data' CSV
file_hdd_cdd <- file.path(DIR_WEATHER, "HDD_CDD Data.csv")

# Read the data from the CSV file
hdd_cdd_data <- read.csv(file_hdd_cdd)

# Convert the Date column in hdd_cdd_data to Date type
hdd_cdd_data$Date <- as.Date(hdd_cdd_data$Date)

# Ensure the Date column in daily_avg_temp is of Date type
daily_avg_temp$Dia <- as.Date(daily_avg_temp$Dia)

# Find the last date in the daily_avg_temp data
last_date_daily_avg_temp <- max(daily_avg_temp$Dia, na.rm = TRUE)

# Filter the hdd_cdd_data to include only new dates
new_data <- subset(hdd_cdd_data, Date > last_date_daily_avg_temp)

# Select only the relevant columns and rename them to match those in daily_avg_temp
new_data <- new_data %>%
  select(Date, HDD, CDD) %>%
  rename(Dia = Date)

# Append the new data to daily_avg_temp (ignoring Avg_Temperature column)
daily_avg_temp <- daily_avg_temp %>%
  select(Dia, HDD, CDD) %>%
  bind_rows(new_data)
#################################################################################

# Extract date from the datetime format
data$Date <- as.Date(data$Date, format = "%d-%m-%Y-%H-%M-%S")

# Calculate the individual heating and cooling loads for Heat Pump 1 and Heat Pump 2
data <- data %>%
  mutate(Heat_Pump1_Heating_Load = ET_BC1_Calor_actual,
         Heat_Pump1_Cooling_Load = ET_BC1_Frio_actual,
         Heat_Pump2_Heating_Load = ET_BC2_Calor_actual,
         Heat_Pump2_Cooling_Load = ET_BC2_Frio_actual)

# Calculate the sum of the variables for overall and each heat pump
data <- data %>%
  mutate(Total_Heating_Load = rowSums(select(., c("ET_BC1_Calor_actual", "ET_BC2_Calor_actual", "ET_BC3_Calor_actual")), na.rm = TRUE),
         Total_Cooling_Load = rowSums(select(., c("ET_BC1_Frio_actual", "ET_BC2_Frio_actual", "ET_BC3_Frio_actual")), na.rm = TRUE),
         Heat_Pump1_Heating_Load = rowSums(select(., c("ET_BC1_Calor_actual")), na.rm = TRUE),
         Heat_Pump1_Cooling_Load = rowSums(select(., c("ET_BC1_Frio_actual")), na.rm = TRUE),
         Heat_Pump2_Heating_Load = rowSums(select(., c("ET_BC2_Calor_actual")), na.rm = TRUE),
         Heat_Pump2_Cooling_Load = rowSums(select(., c("ET_BC2_Frio_actual")), na.rm = TRUE))

# Aggregate the sum for each day for overall and each heat pump
daily_total_loads <- data %>%
  group_by(Date) %>%
  summarize(Total_Heating_Load = sum(Total_Heating_Load, na.rm = TRUE),
            Total_Cooling_Load = sum(Total_Cooling_Load, na.rm = TRUE),
            Heat_Pump1_Heating_Load = sum(Heat_Pump1_Heating_Load, na.rm = TRUE),
            Heat_Pump1_Cooling_Load = sum(Heat_Pump1_Cooling_Load, na.rm = TRUE),
            Heat_Pump2_Heating_Load = sum(Heat_Pump2_Heating_Load, na.rm = TRUE),
            Heat_Pump2_Cooling_Load = sum(Heat_Pump2_Cooling_Load, na.rm = TRUE))

# Merge daily_total_loads with daily_avg_temp data frame
energy_signature_data <- merge(daily_total_loads, daily_avg_temp, by.x = "Date", by.y = "Dia", all.x = TRUE)

# Add a column to identify weekends and weekdays
energy_signature_data$DayType <- ifelse(weekdays(energy_signature_data$Date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

# Function to create and save a plot with uniform y-axis limits
create_and_save_plot <- function(plot, filename, plot_dir, y_limits) {
  plot <- plot + ylim(y_limits)
  ggsave(file.path(plot_dir, filename), plot)
  print(plot)
}

# Determine the minimum and maximum values for the y-axis for heating and cooling loads
y_min_heating <- min(energy_signature_data$Total_Heating_Load, na.rm = TRUE)
y_max_heating <- max(energy_signature_data$Total_Heating_Load, na.rm = TRUE)
y_min_cooling <- min(energy_signature_data$Total_Cooling_Load, na.rm = TRUE)
y_max_cooling <- max(energy_signature_data$Total_Cooling_Load, na.rm = TRUE)

# Set uniform y-axis limits for heating and cooling loads
y_limits_heating <- c(y_min_heating, y_max_heating)
y_limits_cooling <- c(y_min_cooling, y_max_cooling)

# Plot Heating Load vs HDD
plot_heating_hdd <- ggplot(energy_signature_data, aes(x = HDD, y = Total_Heating_Load, color = DayType)) +
  geom_point() +
  labs(x = "HDD", y = "Total Heating Load", title = "Total Heating Load vs HDD") +
  scale_color_manual(values = c("Weekday" = "blue", "Weekend" = "red"))

create_and_save_plot(plot_heating_hdd, "plot_heating_hdd.png", plot_dir, y_limits_heating)

# Plot Cooling Load vs CDD
plot_cooling_cdd <- ggplot(energy_signature_data, aes(x = CDD, y = Total_Cooling_Load, color = DayType)) +
  geom_point() +
  labs(x = "CDD", y = "Total Cooling Load", title = "Total Cooling Load vs CDD") +
  scale_color_manual(values = c("Weekday" = "blue", "Weekend" = "red"))

create_and_save_plot(plot_cooling_cdd, "plot_cooling_cdd.png", plot_dir, y_limits_cooling)

# Plot Heat Pump 1 Heating Load vs HDD
plot_hp1_heating_hdd <- ggplot(energy_signature_data, aes(x = HDD, y = Heat_Pump1_Heating_Load, color = DayType)) +
  geom_point() +
  labs(x = "HDD", y = "Heat Pump 1 Heating Load", title = "Heat Pump 1 Heating Load vs HDD") +
  scale_color_manual(values = c("Weekday" = "blue", "Weekend" = "red"))

create_and_save_plot(plot_hp1_heating_hdd, "plot_hp1_heating_hdd.png", plot_dir, y_limits_heating)

# Plot Heat Pump 1 Cooling Load vs CDD
plot_hp1_cooling_cdd <- ggplot(energy_signature_data, aes(x = CDD, y = Heat_Pump1_Cooling_Load, color = DayType)) +
  geom_point() +
  labs(x = "CDD", y = "Heat Pump 1 Cooling Load", title = "Heat Pump 1 Cooling Load vs CDD") +
  scale_color_manual(values = c("Weekday" = "blue", "Weekend" = "red"))

create_and_save_plot(plot_hp1_cooling_cdd, "plot_hp1_cooling_cdd.png", plot_dir, y_limits_cooling)

# Plot Heat Pump 2 Heating Load vs HDD
plot_hp2_heating_hdd <- ggplot(energy_signature_data, aes(x = HDD, y = Heat_Pump2_Heating_Load, color = DayType)) +
  geom_point() +
  labs(x = "HDD", y = "Heat Pump 2 Heating Load", title = "Heat Pump 2 Heating Load vs HDD") +
  scale_color_manual(values = c("Weekday" = "blue", "Weekend" = "red"))

create_and_save_plot(plot_hp2_heating_hdd, "plot_hp2_heating_hdd.png", plot_dir, y_limits_heating)

# Plot Heat Pump 2 Cooling Load vs CDD
plot_hp2_cooling_cdd <- ggplot(energy_signature_data, aes(x = CDD, y = Heat_Pump2_Cooling_Load, color = DayType)) +
  geom_point() +
  labs(x = "CDD", y = "Heat Pump 2 Cooling Load", title = "Heat Pump 2 Cooling Load vs CDD") +
  scale_color_manual(values = c("Weekday" = "blue", "Weekend" = "red"))

create_and_save_plot(plot_hp2_cooling_cdd, "plot_hp2_cooling_cdd.png", plot_dir, y_limits_cooling)

# Add Month variable to energy_signature_data
energy_signature_data$Month <- format(energy_signature_data$Date, "%Y-%m")

# Monthly versions of existing plots
# Plot Heating Load vs HDD (Monthly)
plot_heating_hdd_monthly_existing <- ggplot(energy_signature_data, aes(x = HDD, y = Total_Heating_Load, color = Month)) +
  geom_point() +
  labs(x = "HDD", y = "Total Heating Load", title = "Total Heating Load vs HDD (Monthly)") +
  scale_color_discrete(name = "Month")

# Display and save the plot
create_and_save_plot(plot_heating_hdd_monthly_existing, "plot_heating_hdd_monthly_existing.png", plot_dir, y_limits_heating)

# Plot Cooling Load vs CDD (Monthly)
plot_cooling_cdd_monthly_existing <- ggplot(energy_signature_data, aes(x = CDD, y = Total_Cooling_Load, color = Month)) +
  geom_point() +
  labs(x = "CDD", y = "Total Cooling Load", title = "Total Cooling Load vs CDD (Monthly)") +
  scale_color_discrete(name = "Month")

# Display and save the plot
create_and_save_plot(plot_cooling_cdd_monthly_existing, "plot_cooling_cdd_monthly_existing.png", plot_dir, y_limits_cooling)

# Monthly versions of existing plots for each heat pump
# Plot Heating Load vs HDD (Heat Pump 1) (Monthly)
plot_hp1_heating_hdd_monthly_existing <- ggplot(energy_signature_data, aes(x = HDD, y = Heat_Pump1_Heating_Load, color = Month)) +
  geom_point() +
  labs(x = "HDD", y = "Heat Pump 1 Heating Load", title = "Heat Pump 1 Heating Load vs HDD (Monthly)") +
  scale_color_discrete(name = "Month")

# Display and save the plot
create_and_save_plot(plot_hp1_heating_hdd_monthly_existing, "plot_hp1_heating_hdd_monthly_existing.png", plot_dir, y_limits_heating)

# Plot Cooling Load vs CDD (Heat Pump 1) (Monthly)
plot_hp1_cooling_cdd_monthly_existing <- ggplot(energy_signature_data, aes(x = CDD, y = Heat_Pump1_Cooling_Load, color = Month)) +
  geom_point() +
  labs(x = "CDD", y = "Heat Pump 1 Cooling Load", title = "Heat Pump 1 Cooling Load vs CDD (Monthly)") +
  scale_color_discrete(name = "Month")

# Display and save the plot
create_and_save_plot(plot_hp1_cooling_cdd_monthly_existing, "plot_hp1_cooling_cdd_monthly_existing.png", plot_dir, y_limits_cooling)

# Plot Heating Load vs HDD (Heat Pump 2) (Monthly)
plot_hp2_heating_hdd_monthly_existing <- ggplot(energy_signature_data, aes(x = HDD, y = Heat_Pump2_Heating_Load, color = Month)) +
  geom_point() +
  labs(x = "HDD", y = "Heat Pump 2 Heating Load", title = "Heat Pump 2 Heating Load vs HDD (Monthly)") +
  scale_color_discrete(name = "Month")

# Display and save the plot
create_and_save_plot(plot_hp2_heating_hdd_monthly_existing, "plot_hp2_heating_hdd_monthly_existing.png", plot_dir, y_limits_heating)

# Plot Cooling Load vs CDD (Heat Pump 2) (Monthly)
plot_hp2_cooling_cdd_monthly_existing <- ggplot(energy_signature_data, aes(x = CDD, y = Heat_Pump2_Cooling_Load, color = Month)) +
  geom_point() +
  labs(x = "CDD", y = "Heat Pump 2 Cooling Load", title = "Heat Pump 2 Cooling Load vs CDD (Monthly)") +
  scale_color_discrete(name = "Month")

# Display and save the plot
create_and_save_plot(plot_hp2_cooling_cdd_monthly_existing, "plot_hp2_cooling_cdd_monthly_existing.png", plot_dir, y_limits_cooling)

# Linear regression for Heating Load vs HDD
heating_lm <- lm(Total_Heating_Load ~ HDD, data = energy_signature_data)

# Extract coefficients for HDD
heating_A_hdd <- coef(heating_lm)[1]
heating_B_hdd <- coef(heating_lm)[2]

# Linear regression for Cooling Load vs CDD
cooling_lm <- lm(Total_Cooling_Load ~ CDD, data = energy_signature_data)

# Extract coefficients for CDD
cooling_A_cdd <- coef(cooling_lm)[1]
cooling_B_cdd <- coef(cooling_lm)[2]

# Display the coefficients
heating_A_hdd
heating_B_hdd
cooling_A_cdd
cooling_B_cdd

# Print out the equations
cat("Heating Load vs HDD equation: ET = ", heating_A_hdd, " + ", heating_B_hdd, " * HDD\n")
cat("Cooling Load vs CDD equation: ET = ", cooling_A_cdd, " + ", cooling_B_cdd, " * CDD\n")

