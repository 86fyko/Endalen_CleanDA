#load packages
library(ggplot2)
library(dplyr)
library(lubridate)

#set working directory
setwd("~/Documents/Uni/Masterarbeit/Data_Analysis_Endalen")

#load data
data <- read.csv("./data/Weather/Adventdalen_Hour.csv")
head(data)

temp <- data[-c(1:4),c(1,6)]
names(temp) <- c("Time", "Temperature")

# Convert the Time column to POSIXct format
temp$Time <- ymd_hms(temp$Time)
average_temperature <- temp %>%
  mutate(Year = lubridate::year(Time))%>% aggregate(as.numeric(Temperature) ~ Year, ., FUN = mean, na.rm=T)
mean(average_temperature$`as.numeric(Temperature)`)

# Create daily mean temperatures
daily_means <- temp %>%
  mutate(Year = lubridate::year(Time),
         Day_of_Year = lubridate::yday(Time)) %>%
  group_by(Year, Day_of_Year) %>%
  summarize(Daily_Mean_Temperature = mean(as.numeric(Temperature), na.rm = TRUE))

# Print or inspect the result
print(daily_means)

# Convert Year and Day_of_Year to a combined date format
daily_means$Date <- as.Date(paste(daily_means$Year, daily_means$Day_of_Year), format="%Y %j")

# Create plot with years on top of each other
setwd("./plots/env/weather/")
ggplot(daily_means, aes(x = Day_of_Year, y = Daily_Mean_Temperature, color=as.factor(Year))) +
  geom_line() +
  labs(x = "Day of Year", y = "Daily Mean Temperature", title = "Daily Mean Temperature Over Time")+
  scale_color_viridis_d()+
  geom_smooth(method = "loess", se = FALSE) +  # Add a trend line
  theme_bw()
ggsave("adventdalen_DayYear.png")


ggplot(temp_df, aes(x = lubridate::yday(Time), y = DailyAvgTemperature, color=as.factor(year(Time)))) +
  geom_line() +
  labs(x = "Day of Year", y = "Daily Mean Temperature", title = "Daily Mean Temperature Over Time")+
  scale_color_viridis_d()+
  geom_smooth(method = "loess", se = FALSE) +  # Add a trend line
  theme_bw()

ggplot(daily_means, aes(x = Day_of_Year, y = Daily_Mean_Temperature, color=as.factor(Year))) +
  #geom_line() +
  labs(x = "Day of Year", y = "Daily Mean Temperature", title = "Daily Mean Temperature Over Time")+
  scale_color_viridis_d()+
  geom_smooth(method = "loess", se = FALSE) +  # Add a trend line
  theme_bw()
ggsave("adventdalen_trend.png")

#create plot over time
ggplot(daily_means, aes(x = Date, y = Daily_Mean_Temperature)) +
  geom_line() +
  labs(x = "Date", y = "Daily Mean Temperature", title = "Daily Mean Temperature Over Time")+
  geom_smooth(method = "loess", se = FALSE, size = 1.5) +  # Add a trend line
  geom_smooth(method="lm")+
  theme_bw()
ggsave("adventdalen_Date.png")

daily_means$Date <- as.Date(paste(daily_means$Year, daily_means$Day_of_Year), format="%Y %j")

# Create ggplot with weekly mean and trend line
library(zoo)
daily_means$Weekly_Mean <- rollmean(daily_means$Daily_Mean_Temperature, k = 14, fill = NA)

temp_df$Weekly_Mean <- rollmean(temp_df$DailyAvgTemperature, k = 14, fill = NA)


# Create ggplot with running mean
ggplot(daily_means, aes(x = Day_of_Year, y = Weekly_Mean, color = as.factor(Year))) +
  geom_line() +
  labs(x = "Date", y = "Weekly Mean Temperature", title = "Temperature (running mean of 14 days) Over Time") +
  scale_color_viridis_d() +
  #  geom_smooth(method = "loess", se = FALSE) +  # You can adjust the method and color
  theme_minimal()
ggsave("adventdalen_2weekmean.png")

ggplot(temp_df, aes(x = lubridate::yday(Time), y = Weekly_Mean, color = as.factor(year(Time)))) +
  geom_line() +
  labs(x = "Date", y = "Weekly Mean Temperature", title = "Temperature (running mean of 14 days) Over Time") +
  scale_color_viridis_d() +
  #  geom_smooth(method = "loess", se = FALSE) +  # You can adjust the method and color
  theme_minimal()
