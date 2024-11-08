#load packages
library(dplyr)
library(purrr)
library(parallel)

# Set your working directory
setwd("/Users/ms/Documents/Uni/Masterarbeit/Data_Analysis_Endalen/data/Weather")
dat_files_directory <- getwd()
# List all .dat files
dat_files <- list.files(dat_files_directory, pattern = "\\.dat$", full.names = TRUE)
overlap<- dat_files[c(799:802,999:1002,1499:1502,1999:2002)]

# Read all data frames into a list
all_data <- mclapply(dat_files, function(file) {
  df <- read.csv(file, header = FALSE)  # Adjust arguments based on your data
  df <- df[-c(1:4), c(1, 4), drop = FALSE]  # Adjust column indices as needed
  names(df) <- c("Time", "Temperature")
  return(df)
}, mc.cores = parallel::detectCores())

# Bind all data frames into one
merged_df <- bind_rows(all_data, .id = "list_id")

# Calculate daily average temperature
averagtemp <- function(df) {
  df %>%
    group_by(Date = as.Date(Time)) %>%
    summarize(DailyAvgTemperature = mean(as.numeric(Temperature), na.rm = TRUE))
}

# Apply the function to the merged data
averagtempadv <- merged_df %>%
  unique() %>%
  averagtemp()

str(averagtempadv)
head(averagtempadv)

write.csv(averagtempadv, "../secondaryData/DailyAvgTempAdventdalen/temperaturedata(all).csv")
