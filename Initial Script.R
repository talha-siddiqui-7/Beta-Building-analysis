library(tidyr)
library(ggplot2)
library(gridExtra)
library(dplyr)

# Specify the file path
file_path <- "D:/Deusto Job/Atelier-Analysis of Beta Buildings/historical_DIGIPEN_from_20240101.csv"

# Read the CSV file
data <- read.csv(file_path)

# Remove rows with NA values in the 'Date' column
data <- data[complete.cases(data$Date), ]

# HEAT PUMP 1

# Convert non-numeric values in the variables to numeric
data$EE_BC1 <- as.numeric(data$EE_BC1)
data$ET_BC1_Frio <- as.numeric(data$ET_BC1_Frio)
data$ET_BC1_Calor <- as.numeric(data$ET_BC1_Calor)


# Remove NA values from the dataset
data <- na.omit(data)


# Convert 'Date' column to POSIXct object and remove NA values
data$Date <- as.POSIXct(data$Date, format = "%d-%m-%Y;%H:%M:%S", tz = "UTC")
data <- data[complete.cases(data$Date), ]

# Remove rows with 'EE_BC1' containing NA or undefined values
data <- data[complete.cases(data$EE_BC1) & data$EE_BC1 != "undefined", ]

# Remove rows with 'ET_BC1_Frio' containing NA or undefined values
data <- data[complete.cases(data$ET_BC1_Frio) & data$ET_BC1_Frio != "undefined", ]

# Remove rows with 'ET_BC1_Calor' containing NA or undefined values
data <- data[complete.cases(data$ET_BC1_Calor) & data$ET_BC1_Calor != "undefined", ]

# Convert cumulative values to actual values
data$EE_BC1_actual <- c(0, diff(data$EE_BC1))
data$ET_BC1_Frio_actual <- c(0, diff(data$ET_BC1_Frio))
data$ET_BC1_Calor_actual <- c(0, diff(data$ET_BC1_Calor))


# Plot EE_BC1 vs Date
plot(data$Date, data$EE_BC1_actual, type = "l", xlab = "Date", ylab = "EE_BC1", main = "EE_BC1 vs Date")

# Plot ET_BC1_Frio vs Date
plot(data$Date, data$ET_BC1_Frio_actual, type = "l", xlab = "Date", ylab = "ET_BC1_Frio", main = "ET_BC1_Frio vs Date")

# Plot ET_BC1_Calor vs Date
plot(data$Date, data$ET_BC1_Calor_actual, type = "l", xlab = "Date", ylab = "ET_BC1_Calor", main = "ET_BC1_Calor vs Date")





# Extract hour from Date column
data$Hour <- as.numeric(format(data$Date, "%H"))

# Aggregate data by Hour
hourly_data <- data %>%
  group_by(Hour) %>%
  summarize(EE_BC1_hourly = mean(EE_BC1_actual, na.rm = TRUE),
            ET_BC1_Frio_hourly = mean(ET_BC1_Frio_actual, na.rm = TRUE),
            ET_BC1_Calor_hourly = mean(ET_BC1_Calor_actual, na.rm = TRUE))

# Plot hourly data
hourly_plots <- list()

# Plot EE_BC1_actual vs Hour
hourly_plots$EE_BC1 <- ggplot(hourly_data, aes(x = Hour, y = EE_BC1_hourly)) +
  geom_line() +
  labs(x = "Hour", y = "EE_BC1", title = "Hourly EE_BC1") +
  theme_minimal()

# Plot ET_BC1_Frio_actual vs Hour
hourly_plots$ET_BC1_Frio <- ggplot(hourly_data, aes(x = Hour, y = ET_BC1_Frio_hourly)) +
  geom_line() +
  labs(x = "Hour", y = "ET_BC1_Frio", title = "Hourly ET_BC1_Frio") +
  theme_minimal()

# Plot ET_BC1_Calor_actual vs Hour
hourly_plots$ET_BC1_Calor <- ggplot(hourly_data, aes(x = Hour, y = ET_BC1_Calor_hourly)) +
  geom_line() +
  labs(x = "Hour", y = "ET_BC1_Calor", title = "Hourly ET_BC1_Calor") +
  theme_minimal()

# Display plots side by side
gridExtra::grid.arrange(hourly_plots$EE_BC1, hourly_plots$ET_BC1_Frio, hourly_plots$ET_BC1_Calor, ncol = 3)


#HEAT PUMP 2

# Convert non-numeric values in the variables to numeric
data$EE_BC2 <- as.numeric(data$EE_BC2)
data$ET_BC2_Frio <- as.numeric(data$ET_BC2_Frio)
data$ET_BC2_Calor <- as.numeric(data$ET_BC2_Calor)


# Remove NA values from the dataset
data <- na.omit(data)


# Convert 'Date' column to POSIXct object and remove NA values
data$Date <- as.POSIXct(data$Date, format = "%d-%m-%Y;%H:%M:%S", tz = "UTC")
data <- data[complete.cases(data$Date), ]

# Remove rows with 'EE_BC2' containing NA or undefined values
data <- data[complete.cases(data$EE_BC2) & data$EE_BC2 != "undefined", ]

# Remove rows with 'ET_BC2_Frio' containing NA or undefined values
data <- data[complete.cases(data$ET_BC2_Frio) & data$ET_BC2_Frio != "undefined", ]

# Remove rows with 'ET_BC2_Calor' containing NA or undefined values
data <- data[complete.cases(data$ET_BC2_Calor) & data$ET_BC2_Calor != "undefined", ]

# Convert cumulative values to actual values
data$EE_BC2_actual <- c(0, diff(data$EE_BC2))
data$ET_BC2_Frio_actual <- c(0, diff(data$ET_BC2_Frio))
data$ET_BC2_Calor_actual <- c(0, diff(data$ET_BC2_Calor))


# Plot EE_BC2 vs Date
plot(data$Date, data$EE_BC2_actual, type = "l", xlab = "Date", ylab = "EE_BC2", main = "EE_BC2 vs Date")

# Plot ET_BC2_Frio vs Date
plot(data$Date, data$ET_BC2_Frio_actual, type = "l", xlab = "Date", ylab = "ET_BC2_Frio", main = "ET_BC2_Frio vs Date")

# Plot ET_BC2_Calor vs Date
plot(data$Date, data$ET_BC2_Calor_actual, type = "l", xlab = "Date", ylab = "ET_BC2_Calor", main = "ET_BC2_Calor vs Date")


# Extract hour from Date column
data$Hour <- as.numeric(format(data$Date, "%H"))

# Aggregate data by Hour
hourly_data <- data %>%
  group_by(Hour) %>%
  summarize(EE_BC2_hourly = mean(EE_BC2_actual, na.rm = TRUE),
            ET_BC2_Frio_hourly = mean(ET_BC2_Frio_actual, na.rm = TRUE),
            ET_BC2_Calor_hourly = mean(ET_BC2_Calor_actual, na.rm = TRUE))

# Plot hourly data
hourly_plots <- list()

# Plot EE_BC2_actual vs Hour
hourly_plots$EE_BC2 <- ggplot(hourly_data, aes(x = Hour, y = EE_BC2_hourly)) +
  geom_line() +
  labs(x = "Hour", y = "EE_BC2", title = "Hourly EE_BC2") +
  theme_minimal()

# Plot ET_BC2_Frio_actual vs Hour
hourly_plots$ET_BC2_Frio <- ggplot(hourly_data, aes(x = Hour, y = ET_BC2_Frio_hourly)) +
  geom_line() +
  labs(x = "Hour", y = "ET_BC2_Frio", title = "Hourly ET_BC2_Frio") +
  theme_minimal()

# Plot ET_BC2_Calor_actual vs Hour
hourly_plots$ET_BC2_Calor <- ggplot(hourly_data, aes(x = Hour, y = ET_BC2_Calor_hourly)) +
  geom_line() +
  labs(x = "Hour", y = "ET_BC2_Calor", title = "Hourly ET_BC2_Calor") +
  theme_minimal()

# Display plots side by side
gridExtra::grid.arrange(hourly_plots$EE_BC2, hourly_plots$ET_BC2_Frio, hourly_plots$ET_BC2_Calor, ncol = 3)

#HEAT PUMP 3

# Convert non-numeric values in the variables to numeric
data$EE_BC3 <- as.numeric(data$EE_BC3)
data$ET_BC3_Frio <- as.numeric(data$ET_BC3_Frio)
data$ET_BC3_Calor <- as.numeric(data$ET_BC3_Calor)


# Remove NA values from the dataset
data <- na.omit(data)


# Convert 'Date' column to POSIXct object and remove NA values
data$Date <- as.POSIXct(data$Date, format = "%d-%m-%Y;%H:%M:%S", tz = "UTC")
data <- data[complete.cases(data$Date), ]

# Remove rows with 'EE_BC3' containing NA or undefined values
data <- data[complete.cases(data$EE_BC3) & data$EE_BC3 != "undefined", ]

# Remove rows with 'ET_BC3_Frio' containing NA or undefined values
data <- data[complete.cases(data$ET_BC3_Frio) & data$ET_BC3_Frio != "undefined", ]

# Remove rows with 'ET_BC3_Calor' containing NA or undefined values
data <- data[complete.cases(data$ET_BC3_Calor) & data$ET_BC3_Calor != "undefined", ]

# Convert cumulative values to actual values
data$EE_BC3_actual <- c(0, diff(data$EE_BC3))
data$ET_BC3_Frio_actual <- c(0, diff(data$ET_BC3_Frio))
data$ET_BC3_Calor_actual <- c(0, diff(data$ET_BC3_Calor))


# Plot EE_BC3 vs Date
plot(data$Date, data$EE_BC3_actual, type = "l", xlab = "Date", ylab = "EE_BC3", main = "EE_BC3 vs Date")

# Plot ET_BC3_Frio vs Date
plot(data$Date, data$ET_BC3_Frio_actual, type = "l", xlab = "Date", ylab = "ET_BC3_Frio", main = "ET_BC3_Frio vs Date")

# Plot ET_BC3_Calor vs Date
plot(data$Date, data$ET_BC3_Calor_actual, type = "l", xlab = "Date", ylab = "ET_BC3_Calor", main = "ET_BC3_Calor vs Date")


# Extract hour from Date column
data$Hour <- as.numeric(format(data$Date, "%H"))

# Aggregate data by Hour
hourly_data <- data %>%
  group_by(Hour) %>%
  summarize(EE_BC3_hourly = mean(EE_BC3_actual, na.rm = TRUE),
            ET_BC3_Frio_hourly = mean(ET_BC3_Frio_actual, na.rm = TRUE),
            ET_BC3_Calor_hourly = mean(ET_BC3_Calor_actual, na.rm = TRUE))

# Plot hourly data
hourly_plots <- list()

# Plot EE_BC3_actual vs Hour
hourly_plots$EE_BC3 <- ggplot(hourly_data, aes(x = Hour, y = EE_BC3_hourly)) +
  geom_line() +
  labs(x = "Hour", y = "EE_BC3", title = "Hourly EE_BC3") +
  theme_minimal()

# Plot ET_BC3_Frio_actual vs Hour
hourly_plots$ET_BC3_Frio <- ggplot(hourly_data, aes(x = Hour, y = ET_BC3_Frio_hourly)) +
  geom_line() +
  labs(x = "Hour", y = "ET_BC3_Frio", title = "Hourly ET_BC3_Frio") +
  theme_minimal()

# Plot ET_BC3_Calor_actual vs Hour
hourly_plots$ET_BC3_Calor <- ggplot(hourly_data, aes(x = Hour, y = ET_BC3_Calor_hourly)) +
  geom_line() +
  labs(x = "Hour", y = "ET_BC3_Calor", title = "Hourly ET_BC3_Calor") +
  theme_minimal()

# Display plots side by side
gridExtra::grid.arrange(hourly_plots$EE_BC3, hourly_plots$ET_BC3_Frio, hourly_plots$ET_BC3_Calor, ncol = 3)


#COPs 
#HEAT PUMP 1

# Calculate COP for BC1
data$COP_BC1 <- (data$ET_BC1_Frio_actual + data$ET_BC1_Calor_actual) / data$EE_BC1_actual
data$COP_BC1[is.nan(data$COP_BC1)] <- 0


#HEAT PUMP 2

# Calculate COP for BC2
data$COP_BC2 <- (data$ET_BC2_Frio_actual + data$ET_BC2_Calor_actual) / data$EE_BC2_actual
data$COP_BC2[is.nan(data$COP_BC2)] <- 0

#HEAT PUMP 3

data$COP_BC3 <- (data$ET_BC3_Frio_actual + data$ET_BC3_Calor_actual) / data$EE_BC3_actual
data$COP_BC3[is.nan(data$COP_BC3)] <- 0


#PLOTS COP vs DAY/WEEK

# Convert Date to the desired format
data$Date_formatted <- format(data$Date, "%d-%m-%Y")


# Plot COP_BC1 vs Date
plot(data$Date, data$COP_BC1, type = "l", xlab = "Date", ylab = "COP_BC1", main = "COP_BC1 vs Date")


# Plot COP_BC2 vs Date
plot(data$Date, data$COP_BC2, type = "l", xlab = "Date", ylab = "COP_BC2", main = "COP_BC2 vs Date")


# Plot COP_BC3 vs Date
plot(data$Date, data$COP_BC3, type = "l", xlab = "Date", ylab = "COP_BC3", main = "COP_BC3 vs Date")

# HOURLY PLOTS
# HEAT PUMP 1
# Extract hour from Date column
data$Hour <- as.numeric(format(data$Date, "%H"))

# Aggregate data by Hour
hourly_data <- data %>%
  group_by(Hour) %>%
  summarize(COP_BC1_hourly = mean(COP_BC1, na.rm = TRUE))

# Plot hourly data for COP_BC1
hourly_plots$COP_BC1 <- ggplot(hourly_data, aes(x = Hour, y = COP_BC1_hourly)) +
  geom_line() +
  labs(x = "Hour", y = "COP_BC1", title = "Hourly COP_BC1") +
  theme_minimal()


print(hourly_plots$COP_BC1)


# HEAT PUMP 2

data$Hour <- as.numeric(format(data$Date, "%H"))

# Aggregate data by Hour
hourly_data <- data %>%
  group_by(Hour) %>%
  summarize(COP_BC2_hourly = mean(COP_BC2, na.rm = TRUE))

# Plot hourly data for COP_BC2
hourly_plots$COP_BC2 <- ggplot(hourly_data, aes(x = Hour, y = COP_BC2_hourly)) +
  geom_line() +
  labs(x = "Hour", y = "COP_BC2", title = "Hourly COP_BC2") +
  theme_minimal()


print(hourly_plots$COP_BC2)

#HEAT PUMP 3

data$Hour <- as.numeric(format(data$Date, "%H"))

# Aggregate data by Hour
hourly_data <- data %>%
  group_by(Hour) %>%
  summarize(COP_BC3_hourly = mean(COP_BC3, na.rm = TRUE))

# Plot hourly data for COP_BC3
hourly_plots$COP_BC3 <- ggplot(hourly_data, aes(x = Hour, y = COP_BC3_hourly)) +
  geom_line() +
  labs(x = "Hour", y = "COP_BC3", title = "Hourly COP_BC3") +
  theme_minimal()


print(hourly_plots$COP_BC3)












