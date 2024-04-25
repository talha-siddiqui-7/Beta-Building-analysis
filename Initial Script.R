library(tidyr)
library(ggplot2)
library(gridExtra)
library(dplyr)

# Set working directory and data directory
WD <- getwd()
DIR_DATA <- file.path(WD, "data")

# Set the directory for saving plots
plot_dir <- file.path(WD, "Plots")

# List all files in the data directory
file_list <- list.files(DIR_DATA)

# Check if there are any files in the directory
if (length(file_list) == 0) {
  stop("No files found in the data directory.")
} else {
  # Assuming you want to read the first file in the list
  file_path <- file.path(DIR_DATA, file_list[1])
  
  # Read the CSV file
  data <- read.csv(file_path)
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

# Iterate over each prefix
for (prefix in prefixes) {
  # Extract hour from Date column
  data$Hour <- as.numeric(format(data$Date, "%H"))
  
  # Aggregate data by Hour
  hourly_data <- data %>%
    group_by(Hour) %>%
    summarize(!!paste0("EE_", prefix, "_hourly") := 
                mean(!!sym(paste0("EE_", prefix, "_actual")), na.rm = TRUE),
              !!paste0("ET_", prefix, "_Frio_hourly") := 
                mean(!!sym(paste0("ET_", prefix, "_Frio_actual")), na.rm = TRUE),
              !!paste0("ET_", prefix, "_Calor_hourly") := 
                mean(!!sym(paste0("ET_", prefix, "_Calor_actual")), na.rm = TRUE))
  
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

# Function for day of the week plots against EE_BC,ET_BC_Frio,EE_BC_Calor
# Define a vector of prefixes
prefixes <- c("BC1", "BC2", "BC3")

# Create an empty list to store the plots
all_plots <- list()

# Iterate over each prefix
for (prefix in prefixes) {
  # Extract day of the week from Date column
  data$DayOfWeek <- wday(data$Date, label = TRUE)
  
  # Aggregate data by day of the week
  hourly_data <- data %>%
    group_by(DayOfWeek) %>%
    summarize(!!paste0("EE_", prefix, "_daily") := 
                mean(!!sym(paste0("EE_", prefix, "_actual")), na.rm = TRUE),
              !!paste0("ET_", prefix, "_Frio_daily") := 
                mean(!!sym(paste0("ET_", prefix, "_Frio_actual")), na.rm = TRUE),
              !!paste0("ET_", prefix, "_Calor_daily") := 
                mean(!!sym(paste0("ET_", prefix, "_Calor_actual")), na.rm = TRUE))
  
  # Plot daily data
  daily_plots <- list()
  
  # Plot EE_actual vs Day of the week
  daily_plots$EE <- ggplot(hourly_data, aes(x = DayOfWeek, y = !!sym(paste0("EE_", prefix, "_daily")))) +
    geom_point() +
    labs(x = "Day of the Week", y = paste("EE_", prefix, "(kwh)"), title = paste("Daily EE_", prefix)) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8))  # Adjust the font size of x-axis text
  
  # Plot ET_Frio_actual vs Day of the week
  daily_plots$ET_Frio <- ggplot(hourly_data, aes(x = DayOfWeek,
                                                 y = !!sym(paste0("ET_", prefix,
                                                                  "_Frio_daily")))) +
    geom_point() +
    labs(x = "Day of the Week", y = paste("ET_", prefix, "_Frio(kwh)"), title =
           paste("Daily ET_", prefix, "_Frio")) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8))  # Adjust the font size of x-axis text
  
  # Plot ET_Calor_actual vs Day of the week
  daily_plots$ET_Calor <- ggplot(hourly_data, aes(x = DayOfWeek, y = !!sym(paste0("ET_", prefix, "_Calor_daily")))) +
    geom_point() +
    labs(x = "Day of the Week", y = paste("ET_", prefix, "_Calor(kwh)"), title = paste("Daily ET_", prefix, "_Calor")) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8))  # Adjust the font size of x-axis text
  
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

# HOURLY COP PLOTS
# function to plot and save COP vs hour
plot_and_save_hourly_cop <- function(data, variable_name, plot_title, plot_dir) {
  # Extract hour from Date column
  data$Hour <- as.numeric(format(data$Date, "%H"))
  
  # Aggregate data by Hour
  hourly_data <- data %>%
    group_by(Hour) %>%
    summarize_at(vars({{variable_name}}), mean, na.rm = TRUE)
  
  # Plot hourly data for COP and store it in a variable
  plot <- ggplot(hourly_data, aes(x = Hour, y = {{variable_name}})) +
    geom_point() +
    labs(x = "Hour", y = plot_title, title = paste("Hourly", plot_title)) +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white"))
  
  # Print the plot
  print(plot)
  
  # Save the plot as a PNG file
  ggsave(file.path(plot_dir, paste(plot_title, "_hourly.png")), plot)
}

# Call the function for COP_BC1
plot_and_save_hourly_cop(data, COP_BC1, "COP_BC1", plot_dir)

# HEAT PUMP 2
plot_and_save_hourly_cop(data, COP_BC2, "COP_BC2", plot_dir)

# HEAT PUMP 3
plot_and_save_hourly_cop(data, COP_BC3, "COP_BC3", plot_dir)

# function to plot and save COP vs day of the week
plot_and_save_daily_cop <- function(data, variable_name, plot_title, plot_dir) {
  # Extract day of the week from Date column
  data$DayOfWeek <- weekdays(data$Date)
  
  # Aggregate data by day of the week
  daily_data <- data %>%
    group_by(DayOfWeek) %>%
    summarize_at(vars({{variable_name}}), mean, na.rm = TRUE)
  
  # Plot daily data for COP and store it in a variable
  plot <- ggplot(daily_data, aes(x = DayOfWeek, y = {{variable_name}})) +
    geom_point() +
    labs(x = "Day of the Week", y = plot_title, title = paste("Daily", plot_title)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  
  # Print the plot
  print(plot)
  
  # Save the plot as a PNG file
  ggsave(file.path(plot_dir, paste(plot_title, "_daily.png")), plot)
}

# Call the function for COP_BC1
plot_and_save_daily_cop(data, COP_BC1, "COP_BC1", plot_dir)

# Call the function for COP_BC2
plot_and_save_daily_cop(data, COP_BC2, "COP_BC2", plot_dir)

# Call the function for COP_BC3
plot_and_save_daily_cop(data, COP_BC3, "COP_BC3", plot_dir)


#comparison of heat pumps hourly energy consumption
# Reshape data into long format
hourly_data_long <- tidyr::pivot_longer(hourly_data, cols = starts_with("EE_BC"),
                                        names_to = "HeatPump", values_to = "EE_actual")

# Plot hourly data for EE_BC1 and EE_BC2
hourly_plot_comparison <- ggplot(hourly_data_long, aes(x = Hour, y = EE_actual, color = HeatPump)) +
  geom_point() +
  labs(x = "Hour", y = "Energy Consumption(kwh)", title = "Hourly Energy Consumption for Heat Pumps") +
  scale_color_manual(values = c("blue", "red")) +  # Set colors for each heat pump
  theme_minimal()

# Save the plot as a PNG file
ggsave(file.path(plot_dir, "COP_BC2_hourly.png"), hourly_plot_comparison)

print(hourly_plot_comparison)

#comparison of heat pumps daily energy consumption

# Reshape data into longer format
data_long <- data %>%
  select(Date, EE_BC1_actual, EE_BC2_actual) %>%  # Remove EE_BC3_actual from the selection
  pivot_longer(cols = -Date, names_to = "HeatPump", values_to = "Energy_Consumption")

# Plot EE_BC1_actual and EE_BC2_actual against Date with different colors
daily_plot_comparison <- ggplot(data_long, aes(x = Date, y = Energy_Consumption, color = HeatPump)) +
  geom_point() +
  labs(x = "Date", y = "Energy Consumption(kwh)", title = "Energy Consumption vs Date by Heat Pump") +
  scale_color_manual(values = c("red", "blue")) +  # Set colors for each heat pump
  theme_minimal()

# Save the plot as a PNG file
ggsave(file.path(plot_dir, "COP_BC2_hourly.png"), daily_plot_comparison)

print(daily_plot_comparison)


# function to plot COP vs ET_BC and save the plot
plot_and_save_cop_vs_et_bc <- function(data, prefix, plot_dir) {
  # Create the new column for ET_BC by summing Frio and Calor columns
  et_bc_column <- paste0("ET_BC", prefix)
  et_bc_frio_column <- paste0("ET_BC", prefix, "_Frio_actual")
  et_bc_calor_column <- paste0("ET_BC", prefix, "_Calor_actual")
  data[[et_bc_column]] <- data[[et_bc_frio_column]] + data[[et_bc_calor_column]]
  
  # Plot COP vs ET_BC
  plot_title <- paste("COP_BC", prefix, " vs ET_BC", prefix)
  plot <- ggplot(data, aes(x = !!sym(et_bc_column), y = !!sym(paste0("COP_BC", prefix)))) +
    geom_point() +
    labs(x = paste(et_bc_column, "(kwh)"), y = paste("COP_BC", prefix), title = plot_title) +
    theme_minimal()
  
  # Print the plot
  print(plot)
  
  # Save the plot as a PNG file
  ggsave(file.path(plot_dir, paste("COP_BC", prefix, "_vs_ET_BC", prefix, ".png")), plot)
}

# Call the function for each heat pump
for (i in 1:2) {
  plot_and_save_cop_vs_et_bc(data, i, plot_dir)
}











