####codes doesn't work needs some debugging####

library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpmisc)

# Read the data from the CSV file
data <- snowmelt_long


# Convert the date column to a proper date format
data$date <- as.Date(data$date)

# Convert date to numeric
data$numeric_date <- as.numeric(data$date)

# Define the sigmoid function
sigmoid <- function(x, Asym, xmid, scal) {
  Asym / (1 + exp((xmid - x) / scal))
}

# Estimate initial parameter values
initial_values <- list(
  Asym = max(data$snow_cover),      # Maximum snow cover as the asymptote
  xmid = median(data$numeric_date), # Median date as the midpoint
  scal = diff(range(data$numeric_date)) / 4  # One-fourth of the date range as the scaling factor
)

# Fit the inverse sigmoid curve using nls model
fit <- nls(snow_cover ~ Asym / (1 + exp((numeric_date - xmid) / scal)), data = data, start = initial_values)

# Generate a sequence of dates for plotting
date_seq <- seq(min(data$date), max(data$date), by = "day")

# Predict snow cover values using the fitted model
predicted <- data.frame(date = date_seq, snow_cover = predict(fit, newdata = data.frame(x = as.numeric(date_seq))))

# Plot the data and the fitted curve
ggplot(data, aes(x = date, y = snow_cover)) +
  geom_point() +
  geom_line(data = predicted, aes(x = date, y = snow_cover), color = "red", size = 1) +
  scale_x_date(date_labels = "%Y-%m-%d") +
  labs(x = "Date", y = "Snow Cover") +
  theme_bw()


# Convert the date column to a proper date format
data$date <- as.Date(data$date)

# Convert date to numeric
data$numeric_date <- as.numeric(data$date)

# Define the sigmoid function
sigmoid <- function(x, Asym, xmid, scal) {
  Asym / (1 + exp((xmid - x) / scal))
}

# Estimate initial parameter values
initial_values <- list(
  Asym = max(data$snow_cover),      # Maximum snow cover as the asymptote
  xmid = median(data$numeric_date), # Median date as the midpoint
  scal = diff(range(data$numeric_date)) / 4  # One-fourth of the date range as the scaling factor
)

# Fit the inverse sigmoid curve using nls model
fit <- nls(snow_cover ~ Asym / (1 + exp((numeric_date - xmid) / scal)), data = data, start = initial_values)

# Generate a sequence of dates for plotting
date_seq <- seq(min(data$date), max(data$date), by = "day")

# Predict snow cover values using the fitted model
predicted <- data.frame(date = date_seq, snow_cover = predict(fit, newdata = data.frame(numeric_date = as.numeric(date_seq))))

# Plot the data and the fitted curve
ggplot(data, aes(x = date, y = snow_cover)) +
  geom_point() +
  geom_line(data = predicted, aes(x = date, y = snow_cover), color = "red", size = 1) +
  scale_x_date(date_labels = "%Y-%m-%d") +
  labs(x = "Date", y = "Snow Cover") +
  theme_bw()


# Convert the date column to a proper date format
data$date <- as.Date(data$date)

# Plot the data with a smooth curve
ggplot(data, aes(x = date, y = snow_cover)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  scale_x_date(date_labels = "%Y-%m-%d") +
  labs(x = "Date", y = "Snow Cover") +
  theme_bw()

