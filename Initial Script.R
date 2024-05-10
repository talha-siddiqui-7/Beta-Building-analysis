library(tidyr)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(lubridate)
library(magrittr)

# Set working directory and data directory
WD <- getwd()
DIR_DATA <- file.path(WD, "data")
DIR_WEATHER <- file.path(WD, "Weather files")

# Set the directory for saving plots
plot_dir <- file.path(WD, "Plots")

# Read energy data file
file_list <- list.files(DIR_DATA)
if (length(file_list) == 0) {
  stop("No files found in the data directory.")
} else {
  file_path <- file.path(DIR_DATA, file_list[1])
  data <- read.csv(file_path)
}

# Read weather data file
weather_file_list <- list.files(DIR_WEATHER, full.names = TRUE)
if (length(weather_file_list) == 0) {
  stop("No files found in the weather directory.")
} else {
  weather_file_path <- weather_file_list[1]
  weather_data <- read.csv(weather_file_path)
}

# Remove NA values from the dataset
data <- na.omit(data)


#Making Daily plots of all heat pumps against EE,ET_Frio,ET_Calor
# Convert non-numeric values in the variables to numeric
columns_to_convert <- c("EE_BC1", "ET_BC1_Frio", "ET_BC1_Calor",
                        "EE_BC2", "ET_BC2_Frio", "ET_BC2_Calor",
                        "EE_BC3", "ET_BC3_Frio", "ET_BC3_Calor")

for (col in columns_to_convert) {
  data[[col]] <- as.numeric(data[[col]])
}

# Convert energy values from MWh to kWh
columns_to_convert <- c("ET_BC1_Frio", "ET_BC1_Calor",
                        "ET_BC2_Frio", "ET_BC2_Calor",
                        "ET_BC3_Frio", "ET_BC3_Calor")

for (col in columns_to_convert) {
  data[[col]] <- data[[col]] * 1000
}

# function to process each heat pump
process_heat_pump <- function(data, prefix) {
  # Remove NA values from the dataset
  data <- na.omit(data)

  # Convert 'Date' column to POSIXct object
  data$Date <- as.POSIXct(data$Date, format = "%d-%m-%Y;%H:%M:%S", tz = "UTC")
  data <- data[complete.cases(data$Date), ]

  # Select columns for the current heat pump
  pump_cols <- grep(paste0("^EE_BC", prefix, "|^ET_BC", prefix, "_Frio|^ET_BC",
                           prefix, "_Calor"), names(data), value = TRUE)

  # Convert cumulative values to actual values for the current heat pump
  actual_cols <- grep(paste0("^EE_BC", prefix, "|^ET_BC", prefix, "_Frio|^ET_BC",
                             prefix, "_Calor"), names(data), value = TRUE)
  for (col in actual_cols) {
    data[[paste0(col, "_actual")]] <- c(0, diff(data[[col]]))
  }

  return(data)
}


# function to plot and save images for a specific heat pump
plot_and_save_image <- function(data, prefix, plot_dir) {

  # Plot EE_BC vs Date
  ee_plot <- ggplot(data, aes(x = Date, y = !!sym(paste0("EE_BC",
                                                         prefix, "_actual")))) +
    geom_point() +
    labs(x = "Date", y = paste("EE_BC", prefix, "(kwh)"), title = paste("EE_BC",
                                                                        prefix,
                                                                        "vs Date"))

  # Save the plot as a PNG file
  ggsave(file.path(plot_dir, paste("EE_BC", prefix, "_vs_Date.png")), ee_plot)
  print(ee_plot)

  # Plot ET_BC_Frio vs Date
  et_frio_plot <- ggplot(data, aes(x = Date, y = !!sym(paste0("ET_BC", prefix,
                                                              "_Frio_actual")))) +
    geom_point() +
    labs(x = "Date", y = paste("ET_BC", prefix, "_Frio(kwh)"),
         title = paste("ET_BC", prefix, "_Frio vs Date"))

  # Save the plot as a PNG file
  ggsave(file.path(plot_dir, paste("ET_BC", prefix,
                                   "_Frio_vs_Date.png")), et_frio_plot)
  print(et_frio_plot)

  # Plot ET_BC_Calor vs Date
  et_calor_plot <- ggplot(data, aes(x = Date, y = !!sym(paste0("ET_BC",
                                                               prefix, "_Calor_actual")))) +
    geom_point() +
    labs(x = "Date", y = paste("ET_BC", prefix, "_Calor(kwh)"),
         title = paste("ET_BC", prefix, "_Calor vs Date"))

  # Save the plot as a PNG file
  ggsave(file.path(plot_dir, paste("ET_BC", prefix, "_Calor_vs_Date.png")),
         et_calor_plot)
  print(et_calor_plot)
}

# Process each heat pump and plot/save images
for (i in 1:3) {
  data <- process_heat_pump(data, i)

  # Plot and save images for each heat pump
  plot_and_save_image(data, i, plot_dir)
}

#Making hourly plots of all heat pumps against EE,ET_Frio,ET_Calor
# Define a vector of prefixes
prefixes <- c("BC1", "BC2", "BC3")

# Create an empty list to store the plots
all_plots <- list()

# Create an empty data frame to store the total_hourly_thermal_load
total_hourly_thermal_load <- data.frame(Hour = 0:23, Heat_Pump_1 = numeric(24), Heat_Pump_2 = numeric(24))

# Iterate over each prefix
for (prefix in prefixes) {
  # Extract hour from Date column
  data$Hour <- as.numeric(format(data$Date, "%H"))
  
  # Aggregate data by Hour
  hourly_data <- data %>%
    group_by(Hour) %>%
    summarize(
      !!paste0("EE_", prefix, "_hourly") :=
        mean(!!sym(paste0("EE_", prefix, "_actual")), na.rm = TRUE),
      !!paste0("ET_", prefix, "_Frio_hourly") :=
        mean(!!sym(paste0("ET_", prefix, "_Frio_actual")), na.rm = TRUE),
      !!paste0("ET_", prefix, "_Calor_hourly") :=
        mean(!!sym(paste0("ET_", prefix, "_Calor_actual")), na.rm = TRUE)
    )
  
  # Calculate total thermal load for Heat Pump 1 and Heat Pump 2
  if (prefix == "BC1") {
    total_hourly_thermal_load$Heat_Pump_1 <- hourly_data$ET_BC1_Frio_hourly + hourly_data$ET_BC1_Calor_hourly
  } else if (prefix == "BC2") {
    total_hourly_thermal_load$Heat_Pump_2 <- hourly_data$ET_BC2_Frio_hourly + hourly_data$ET_BC2_Calor_hourly
  }
  
  # Plot hourly data
  hourly_plots <- list()
  
  # Plot EE_actual vs Hour
  hourly_plots$EE <- ggplot(hourly_data, aes(x = Hour, y = !!sym(paste0("EE_", prefix, "_hourly")))) +
    geom_point() +
    labs(x = "Hour", y = paste("EE_", prefix, "(kwh)"), title = paste("Hourly EE_", prefix)) +
    theme_minimal()
  
  # Plot ET_Frio_actual vs Hour
  hourly_plots$ET_Frio <- ggplot(hourly_data, aes(x = Hour,
                                                  y = !!sym(paste0("ET_", prefix,
                                                                   "_Frio_hourly")))) +
    geom_point() +
    labs(x = "Hour", y = paste("ET_", prefix, "_Frio(kwh)"), title =
           paste("Hourly ET_", prefix, "_Frio")) +
    theme_minimal()
  
  # Plot ET_Calor_actual vs Hour
  hourly_plots$ET_Calor <- ggplot(hourly_data, aes(x = Hour, y = !!sym(paste0("ET_", prefix, "_Calor_hourly")))) +
    geom_point() +
    labs(x = "Hour", y = paste("ET_", prefix, "_Calor(kwh)"), title = paste("Hourly ET_", prefix, "_Calor")) +
    theme_minimal()
  
  # Store the side-by-side plot in a variable
  side_by_side_plot <- gridExtra::grid.arrange(hourly_plots$EE, hourly_plots$ET_Frio, hourly_plots$ET_Calor, ncol = 3)
  
  # Store the plot in the list
  all_plots[[prefix]] <- side_by_side_plot
}

# Save the arranged plots as PNG files
for (i in 1:length(prefixes)) {
  ggsave(file.path(plot_dir, paste("EE_", prefixes[i], "_and_ET_", prefixes[i], "_plots.png")), all_plots[[i]])
}

# Define a vector of prefixes
prefixes <- c("BC1", "BC2", "BC3")

# Create an empty list to store the plots
all_plots <- list()

# Create an empty data frame to store the total_daily_thermal_load
total_daily_thermal_load <- data.frame(DayOfWeek = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
                                       Heat_Pump_1 = numeric(7),
                                       Heat_Pump_2 = numeric(7))

# Iterate over each prefix
for (prefix in prefixes) {
  # Extract day of the week from Date column
  data$DayOfWeek <- wday(data$Date, label = TRUE)
  
  # Aggregate data by day of the week
  daily_data <- data %>%
    group_by(DayOfWeek) %>%
    summarize(
      !!paste0("EE_", prefix, "_daily") :=
        mean(!!sym(paste0("EE_", prefix, "_actual")), na.rm = TRUE),
      !!paste0("ET_", prefix, "_Frio_daily") :=
        mean(!!sym(paste0("ET_", prefix, "_Frio_actual")), na.rm = TRUE),
      !!paste0("ET_", prefix, "_Calor_daily") :=
        mean(!!sym(paste0("ET_", prefix, "_Calor_actual")), na.rm = TRUE)
    )
  
  # Calculate total thermal load for Heat Pump 1 and Heat Pump 2
  if (prefix == "BC1") {
    total_daily_thermal_load$Heat_Pump_1 <- daily_data$ET_BC1_Frio_daily + daily_data$ET_BC1_Calor_daily
  } else if (prefix == "BC2") {
    total_daily_thermal_load$Heat_Pump_2 <- daily_data$ET_BC2_Frio_daily + daily_data$ET_BC2_Calor_daily
  }
  
  # Plot daily data
  daily_plots <- list()
  
  # Plot EE_actual vs Day of the week
  daily_plots$EE <- ggplot(daily_data, aes(x = DayOfWeek, y = !!sym(paste0("EE_", prefix, "_daily")))) +
    geom_point() +
    labs(x = "Day of the Week", y = paste("EE_", prefix, "(kwh)"), title = paste("Daily EE_", prefix)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  
  # Plot ET_Frio_actual vs Day of the week
  daily_plots$ET_Frio <- ggplot(daily_data, aes(x = DayOfWeek,
                                                y = !!sym(paste0("ET_", prefix,
                                                                 "_Frio_daily")))) +
    geom_point() +
    labs(x = "Day of the Week", y = paste("ET_", prefix, "_Frio(kwh)"), title =
           paste("Daily ET_", prefix, "_Frio")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  
  # Plot ET_Calor_actual vs Day of the week
  daily_plots$ET_Calor <- ggplot(daily_data, aes(x = DayOfWeek, y = !!sym(paste0("ET_", prefix, "_Calor_daily")))) +
    geom_point() +
    labs(x = "Day of the Week", y = paste("ET_", prefix, "_Calor(kwh)"), title = paste("Daily ET_", prefix, "_Calor")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  
  # Store the side-by-side plot in a variable
  side_by_side_plot <- gridExtra::grid.arrange(daily_plots$EE, daily_plots$ET_Frio, daily_plots$ET_Calor, ncol = 3)
  
  # Store the plot in the list
  all_plots[[prefix]] <- side_by_side_plot
}

# Save the arranged plots as PNG files
for (i in 1:length(prefixes)) {
  ggsave(file.path(plot_dir, paste("EE_", prefixes[i], "_and_ET_", prefixes[i], "_plots.png")), all_plots[[i]])
}

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

#PLOTS COP vs DAY/WEEK

# Function to plot and save COP vs Date
plot_and_save_cop <- function(data, col_name, plot_dir) {
  plot <- ggplot(data, aes(x = Date, y = !!sym(col_name))) +
    geom_point() +
    labs(x = "Date", y = col_name, title = paste(col_name, "vs Date"))

  ggsave(file.path(plot_dir, paste(col_name, "_vs_Date.png")), plot)
  print(plot)
}

# Convert Date to the desired format
data$Date_formatted <- format(data$Date, "%d-%m-%Y")

# Plot and save COP for each column
cols_to_plot <- c("COP_BC1", "COP_BC2", "COP_BC3")
for (col in cols_to_plot) {
  plot_and_save_cop(data, col, plot_dir)
}

# Function to plot and save COP vs hour
plot_and_save_hourly_cop <- function(data, variable_name, plot_title, plot_dir) {
  # Extract hour from Date column
  data$Hour <- as.numeric(format(data$Date, "%H"))
  
  # Aggregate data by Hour
  hourly_data <- data %>%
    group_by(Hour) %>%
    summarize_at(vars({{variable_name}}), mean, na.rm = TRUE)
  
  # Save the hourly COP values in a data frame
  cop_values <- hourly_data[[2]]  # Extracting the COP values column
  
  # Create a data frame with Hour and COP values
  hourly_cop_df <- data.frame(Hour = hourly_data$Hour, COP = cop_values)
  
  # Plot hourly data for COP and store it in a variable
  plot <- ggplot(hourly_data, aes(x = Hour, y = {{variable_name}})) +
    geom_point() +
    labs(x = "Hour", y = plot_title, title = paste("Hourly", plot_title)) +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white")) +
    ylim(0, 10)  # Set y-axis limits from 1 to 10
  
  # Print the plot
  print(plot)
  
  # Save the plot as a PNG file
  ggsave(file.path(plot_dir, paste(plot_title, "_hourly.png")), plot)
  
  # Return the data frame with Hour and COP values
  return(hourly_cop_df)
}

# Call the function for COP_BC1
hourly_cop_BC1 <- plot_and_save_hourly_cop(data, COP_BC1, "COP_BC1", plot_dir)

# Call the function for COP_BC2
hourly_cop_BC2 <- plot_and_save_hourly_cop(data, COP_BC2, "COP_BC2", plot_dir)

# Call the function for COP_BC3
hourly_cop_BC3 <- plot_and_save_hourly_cop(data, COP_BC3, "COP_BC3", plot_dir)

# Function to plot and save COP vs day of the week
plot_and_save_daily_cop <- function(data, variable_name, plot_title, plot_dir) {
  # Extract day of the week from Date column
  data$DayOfWeek <- factor(weekdays(data$Date), levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
  
  # Aggregate data by day of the week
  daily_data <- data %>%
    group_by(DayOfWeek) %>%
    summarize_at(vars({{variable_name}}), mean, na.rm = TRUE)
  
  # Save the daily COP values in a data frame
  cop_values <- daily_data[[2]]  # Extracting the COP values column
  # Create a data frame with Day of the Week and COP values
  daily_cop_df <- data.frame(DayOfWeek = daily_data$DayOfWeek, COP = cop_values)
  
  # Plot daily data for COP and store it in a variable
  plot <- ggplot(daily_data, aes(x = DayOfWeek, y = {{variable_name}})) +
    geom_point() +
    labs(x = "Day of the Week", y = plot_title, title = paste("Daily", plot_title)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
    ylim(0, 10)  # Set y-axis limits from 1 to 10
  
  # Print the plot
  print(plot)
  
  # Save the plot as a PNG file
  ggsave(file.path(plot_dir, paste(plot_title, "_daily.png")), plot)
  
  # Return the data frame with Day of the Week and COP values
  return(daily_cop_df)
}

# Call the function for COP_BC1
daily_cop_BC1 <- plot_and_save_daily_cop(data, COP_BC1, "COP_BC1", plot_dir)

# Call the function for COP_BC2
daily_cop_BC2 <- plot_and_save_daily_cop(data, COP_BC2, "COP_BC2", plot_dir)

# Call the function for COP_BC3
daily_cop_BC3 <- plot_and_save_daily_cop(data, COP_BC3, "COP_BC3", plot_dir)


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
# Convert Dia column to Date format if it's not already in Date format
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

#calculating average thermal loads
# Extract date from the datetime format
data$Date <- as.Date(data$Date, format = "%d-%m-%Y-%H-%M-%S")

# Calculate the individual loads for Heat Pump 1 and Heat Pump 2
data <- data %>%
  mutate(Heat_Pump1_Load = ET_BC1_Frio_actual + ET_BC1_Calor_actual,
         Heat_Pump2_Load = ET_BC2_Frio_actual + ET_BC2_Calor_actual)

# Calculate the sum of the variables for overall and each heat pump
data <- data %>%
  mutate(Total_ET = rowSums(select(., c("ET_BC1_Frio_actual", "ET_BC1_Calor_actual",
                                        "ET_BC2_Frio_actual", "ET_BC2_Calor_actual",
                                        "ET_BC3_Frio_actual", "ET_BC3_Calor_actual")), na.rm = TRUE),
         Heat_Pump1_Load = rowSums(select(., c("ET_BC1_Frio_actual", "ET_BC1_Calor_actual")), na.rm = TRUE),
         Heat_Pump2_Load = rowSums(select(., c("ET_BC2_Frio_actual", "ET_BC2_Calor_actual")), na.rm = TRUE))

# Aggregate the sum for each day for overall and each heat pump
daily_total_ET <- data %>%
  group_by(Date) %>%
  summarize(Total_ET = sum(Total_ET, na.rm = TRUE),
            Heat_Pump1_Load = sum(Heat_Pump1_Load, na.rm = TRUE),
            Heat_Pump2_Load = sum(Heat_Pump2_Load, na.rm = TRUE))

# Calculate the average thermal load for each day
daily_avg_ET <- daily_total_ET %>%
  mutate(Avg_ET = Total_ET / n())  # Calculate average by dividing by number of observations

# Create a new data frame with Date, Avg_ET, Heat_Pump1_Load, and Heat_Pump2_Load
daily_avg_ET_df <- data.frame(Date = as.Date(daily_avg_ET$Date),
                              Avg_ET = daily_avg_ET$Avg_ET,
                              Heat_Pump1_Load = daily_avg_ET$Heat_Pump1_Load,
                              Heat_Pump2_Load = daily_avg_ET$Heat_Pump2_Load)

# Merge daily_avg_ET_df with daily_avg_temp data frame
energy_signature_data <- merge(daily_avg_ET_df, daily_avg_temp, by.x = "Date", by.y = "Dia", all.x = TRUE)

# Function to create and save a plot
create_and_save_plot <- function(plot, filename, plot_dir) {
  ggsave(file.path(plot_dir, filename), plot)
  print(plot)
}

# Plot Avg_ET vs HDD
plot_hdd_vs_avg_et <- ggplot(energy_signature_data, aes(x = HDD, y = Avg_ET)) +
  geom_point() +
  labs(x = "HDD", y = "Avg_ET", title = "Avg_ET vs HDD")

create_and_save_plot(plot_hdd_vs_avg_et, "plot_hdd_vs_avg_et.png", plot_dir)

# Plot Avg_ET vs CDD
plot_cdd_vs_avg_et <- ggplot(energy_signature_data, aes(x = CDD, y = Avg_ET)) +
  geom_point() +
  labs(x = "CDD", y = "Avg_ET", title = "Avg_ET vs CDD")

create_and_save_plot(plot_cdd_vs_avg_et, "plot_cdd_vs_avg_et.png", plot_dir)

# Plot Heat Pump 1 Load vs HDD
plot_hp1_hdd <- ggplot(energy_signature_data, aes(x = HDD, y = Heat_Pump1_Load)) +
  geom_point() +
  labs(x = "HDD", y = "Heat_Pump1_Load", title = "Heat Pump 1 Load vs HDD")

create_and_save_plot(plot_hp1_hdd, "plot_hp1_hdd.png", plot_dir)

# Plot Heat Pump 1 Load vs CDD
plot_hp1_cdd <- ggplot(energy_signature_data, aes(x = CDD, y = Heat_Pump1_Load)) +
  geom_point() +
  labs(x = "CDD", y = "Heat_Pump1_Load", title = "Heat Pump 1 Load vs CDD")

create_and_save_plot(plot_hp1_cdd, "plot_hp1_cdd.png", plot_dir)

# Plot Heat Pump 2 Load vs HDD
plot_hp2_hdd <- ggplot(energy_signature_data, aes(x = HDD, y = Heat_Pump2_Load)) +
  geom_point() +
  labs(x = "HDD", y = "Heat_Pump2_Load", title = "Heat Pump 2 Load vs HDD")

create_and_save_plot(plot_hp2_hdd, "plot_hp2_hdd.png", plot_dir)

# Plot Heat Pump 2 Load vs CDD
plot_hp2_cdd <- ggplot(energy_signature_data, aes(x = CDD, y = Heat_Pump2_Load)) +
  geom_point() +
  labs(x = "CDD", y = "Heat_Pump2_Load", title = "Heat Pump 2 Load vs CDD")

create_and_save_plot(plot_hp2_cdd, "plot_hp2_cdd.png", plot_dir)

# Function to plot hourly COP vs hourly thermal load for each heat pump
plot_and_save_hourly_cop_vs_thermal_load <- function(hourly_cop_data, hourly_thermal_load_data, prefix, plot_dir) {
  # Define column names for COP and thermal load for the specified heat pump
  cop_column <- "COP"
  thermal_load_column <- paste0("Heat_Pump_", prefix)
  
  # Merge hourly COP and thermal load data
  merged_data <- merge(hourly_cop_data, hourly_thermal_load_data, by.x = "Hour", by.y = "Hour", all = TRUE)
  
  # Plot hourly COP vs hourly thermal load
  plot_title <- paste("Hourly COP vs Hourly Thermal Load for Heat Pump", prefix)
  plot <- ggplot(merged_data, aes(x = !!sym(thermal_load_column), y = !!sym(cop_column))) +
    geom_point() +
    labs(x = paste("Hourly Thermal Load for Heat Pump", prefix, "(kwh)"), y = paste("COP for Heat Pump", prefix), title = plot_title) +
    theme_minimal()
  
  # Print the plot
  print(plot)
  
  # Save the plot as a PNG file
  ggsave(file.path(plot_dir, paste("Hourly_COP_vs_Thermal_Load_Heat_Pump", prefix, ".png")), plot)
}

# Function to plot daily COP vs daily thermal load for each heat pump
plot_and_save_daily_cop_vs_thermal_load <- function(daily_cop_data, daily_thermal_load_data, prefix, plot_dir) {
  # Define column names for COP and thermal load for the specified heat pump
  cop_column <- "COP"  # Updated to match the actual column name for daily COP
  thermal_load_column <- paste0("Heat_Pump_", prefix)
  
  # Merge daily COP and thermal load data
  merged_data <- merge(daily_cop_data, daily_thermal_load_data, by.x = "DayOfWeek", by.y = "DayOfWeek", all = TRUE)
  
  # Plot daily COP vs daily thermal load
  plot_title <- paste("Daily COP vs Daily Thermal Load for Heat Pump", prefix)
  plot <- ggplot(merged_data, aes(x = !!sym(thermal_load_column), y = !!sym(cop_column))) +
    geom_point() +
    labs(x = paste("Daily Thermal Load for Heat Pump", prefix, "(kwh)"), y = paste("COP for Heat Pump", prefix), title = plot_title) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  
  # Print the plot
  print(plot)
  
  # Save the plot as a PNG file
  ggsave(file.path(plot_dir, paste("Daily_COP_vs_Thermal_Load_Heat_Pump", prefix, ".png")), plot)
}


# Call the functions for each heat pump
plot_and_save_hourly_cop_vs_thermal_load(hourly_cop_data = hourly_cop_BC1,
                                         hourly_thermal_load_data = total_hourly_thermal_load,
                                         prefix = 1, plot_dir = plot_dir)
plot_and_save_daily_cop_vs_thermal_load(daily_cop_data = daily_cop_BC1,
                                        daily_thermal_load_data = total_daily_thermal_load,
                                        prefix = 1, plot_dir = plot_dir)

plot_and_save_hourly_cop_vs_thermal_load(hourly_cop_data = hourly_cop_BC2,
                                         hourly_thermal_load_data = total_hourly_thermal_load,
                                         prefix = 2, plot_dir = plot_dir)
plot_and_save_daily_cop_vs_thermal_load(daily_cop_data = daily_cop_BC2,
                                        daily_thermal_load_data = total_daily_thermal_load,
                                        prefix = 2, plot_dir = plot_dir)
