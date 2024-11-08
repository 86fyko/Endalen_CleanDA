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

# Create daily mean temperatures
daily_means <- temp %>%
  mutate(Date = as.Date(Time),  # Create a Date column
         Year = lubridate::year(Time),
         Day_of_Year = lubridate::yday(Time)) %>%
  group_by(Date, Year, Day_of_Year) %>%
  summarize(Daily_Mean_Temperature = mean(as.numeric(Temperature), na.rm = TRUE))


setwd("/Users/ms/Documents/Uni/Masterarbeit/Data_Analysis_Endalen/data/secondaryData")
#load old data
datdir <- ("./DailyAvgTempAdventdalen/")
getwd()
# List all .dat files
daytemp_files <- list.files(datdir, pattern =  "*csv", full.names = TRUE)
daytemp_files

daytemp.list <- lapply(daytemp_files, read.csv, sep=",")
names(daytemp.list) <- daytemp_files

# Function to remove the first and last entry from each data frame in the list
remove_first_last <- function(df) {
  df[-c(1, nrow(df)), ]
}

# Apply the function to each element of the list
daytemps <- lapply(daytemp.list[-length(daytemp.list)], remove_first_last)%>%
  bind_rows(., .id = "Name") %>%
  arrange(as.Date(Date))

print_first_last <- function(df) {
  df[c(1, nrow(df)), ]
}
first_lasts <- lapply(daytemp.list[-length(daytemp.list)], print_first_last)%>%
  bind_rows(., .id = "Name") %>%
  arrange(as.Date(Date))

overlap <- daytemp.list[[length(daytemp.list)]]
#last_daytemp[last_daytemp$Date %in% unique_dates_last, ]
overlap[overlap$Date%in% unique(first_lasts$Date),]

daytemp_df <- rbind(first_lasts[1,c(3:4)],daytemps[,c(3:4)],overlap[,c(2:3)])
daytemp_df$Year <-  lubridate::year(daytemp_df$Date)
daytemp_df$Day_of_Year <-  lubridate::yday(daytemp_df$Date)
####merge the different daily means####
names(daytemp_df)[2] <- "Daily_Mean_Temperature"
str(daytemp_df)
daytemp_df$Date <- as.Date(daytemp_df$Date)
names(daily_means)
str(ungroup(daily_means))
allaverage_temp <- merge(daytemp_df,ungroup(daily_means), all=T)

#calculate the average temperature
average_temperature <- allaverage_temp %>% aggregate(as.numeric(Daily_Mean_Temperature) ~ Year, ., FUN = mean, na.rm=T)
names(average_temperature) <- c("Year", "Temperature")
mean(average_temperature$Temperature)


# Create plot with years on top of each other
setwd("~/Documents/Uni/Masterarbeit/Data_Analysis_Endalen/plots/env/weather/")
ggplot(average_temperature, aes(x=Year, y=Temperature))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw()
tempyearlm <- lm(data=average_temperature, Temperature ~Year)
summary(tempyearlm)

ggplot(allaverage_temp, aes(x = Day_of_Year, y = Daily_Mean_Temperature, color=as.factor(Year))) +
  geom_line() +
  labs(x = "Day of Year", y = "Daily Mean Temperature", title = "Daily Mean Temperature Over Time")+
  scale_color_viridis_d()+
  geom_smooth(method = "loess", se = FALSE) +  # Add a trend line
  theme_bw()
ggsave("adventdalen_DayYear.png")

ggplot(allaverage_temp, aes(x = Day_of_Year, y = Daily_Mean_Temperature, color=as.factor(Year))) +
  #geom_line() +
  labs(x = "Day of Year", y = "Daily Mean Temperature", title = "Daily Mean Temperature Over Time")+
  scale_color_viridis_d()+
  geom_smooth(method = "loess", se = FALSE) +  # Add a trend line
  theme_bw()
ggsave("adventdalen_trend.png")

#create plot over time
ggplot(allaverage_temp, aes(x = Date, y = Daily_Mean_Temperature)) +
  geom_line() +
  labs(x = "Date", y = "Daily Mean Temperature", title = "Daily Mean Temperature Over Time")+
  geom_smooth(method = "loess", se = FALSE, size = 1.5) +  # Add a trend line
  geom_smooth(method="lm")+
  theme_bw()
ggsave("adventdalen_Date.png")

tempylm <- lm(data=allaverage_temp, Daily_Mean_Temperature ~Date)
summary(tempylm)

#daily_means$Date <- as.Date(paste(daily_means$Year, daily_means$Day_of_Year), format="%Y %j")

# Create ggplot with weekly mean and trend line
library(zoo)
allaverage_temp$Weekly_Mean <- rollmean(allaverage_temp$Daily_Mean_Temperature, k = 14, fill = NA)

# Create ggplot with running mean
ggplot(allaverage_temp, aes(x = Day_of_Year, y = Weekly_Mean, color = as.factor(Year))) +
  geom_line() +
  labs(x = "Date", y = "Weekly Mean Temperature", title = "Temperature (running mean of 14 days) Over Time") +
  scale_color_viridis_d() +
#  geom_smooth(method = "loess", se = FALSE) +  # You can adjust the method and color
  theme_minimal()
ggsave("adventdalen_2weekmean.png")
