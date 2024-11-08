setwd("~/Documents/Uni/Masterarbeit/Data_Analysis_Endalen/code")
setwd("../data/env/TOMSTdata")

#####TODO: merge series numbers with plot id####


library(tidyverse)
library(data.table)
library(readxl)
library(lubridate)

####Load .csv files into list####
file.list <- list.files(pattern='data*')
df.list <- lapply(file.list, read.csv, sep=";") #gdata::read.xls
#read_xlsx("../data/env/TOMST_logger.xlsx")
Tomst_meta <- read_excel("~/Documents/Uni/Masterarbeit/Data_Analysis_Endalen/data/Env/Tomst_Loggers.xlsx")
treatment <- read_csv("~/Documents/Uni/Masterarbeit/Data_Analysis_Endalen/data/plot_treatments.csv")
Tomst_meta <- merge(Tomst_meta, treatment,by.x="Plot",by.y = "PLOT", all.x = T)
Tomst_meta$SUBSITE <- substr(Tomst_meta$Plot, 1, 5)

names(df.list) <- gsub("data_(\\d+)_\\d+\\.csv", "\\1", file.list) #name the files


# Print the extracted numbers
#print(file_numbers)


names<- c("index_of_measure", "datetime", "time_zone", "T1", "T2","T3","soil_moisture", "shake","errFlag")
for(i in 1:length(df.list)){
  names(df.list[[i]]) <- names 
}


####make one dataframe####
TOMSTdata.df <- bind_rows(df.list, .id = "Number")
TOMSTdata.df$datetime <- ymd_hm(TOMSTdata.df$datetime)

#find start date of measurements
start <- filter(TOMSTdata.df, T1 > 20) 
max(start$datetime)
TOMSTdata.dff <- filter(TOMSTdata.df,as.Date(datetime) > "2023-06-22 10:00:00 UTC")


TOMST.df <- merge(TOMSTdata.dff,Tomst_meta, all.x=T)

TOMST.df <- subset(TOMST.df, T3 != min(TOMST.df$T3)) #excludes error
getwd()
#read all the different environmental data
setwd("/Users/ms/Documents/Uni/Masterarbeit/Data_Analysis_Endalen/data/secondaryData/ENV")
write.csv(TOMST.df, "./TOMST_clean.csv",row.names = FALSE) # saves the clean dataframe so I can use it for other analysis
####plot with running mean####

setwd("/Users/ms/Documents/Uni/Masterarbeit/Data_Analysis_Endalen/")
variable_to_plot <- "T2"
TOMST.df <- TOMST.df %>%
  group_by(Number)%>%
  arrange(datetime) %>%
  mutate(RunningMean = zoo::rollmean(!!as.name(variable_to_plot), k = 4, fill = NA))

ggplot(TOMST.df, aes(x = datetime, y = RunningMean,color=Plot)) +
  geom_line(alpha=0.5) +
  scale_colour_viridis_d(
    option = "D",
    aesthetics = "colour",
    name="Plot"
  )+
  labs(title = "Running Mean over 3 Days",
       x = "Date",
       y = "Running Mean") +
  #scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 day") +
  theme_bw()+
  theme(
    text = element_text(size = 16),  # Increase font size for all text
    #   axis.text.y = element_text(size = 16)  # Adjust y-axis text size
  ) 
ggsave(paste("./plots/env/running_mean_plots",variable_to_plot,".png", sep="_" ))

ggplot(TOMST.df, aes(x = datetime, y = RunningMean,color=TREATMENT)) +
  geom_line(alpha=0.5) +
  scale_colour_viridis_d(
    option = "D",
    aesthetics = "colour",
    name="Treatment"
  )+
  labs(title = "Running Mean over 1 hour",
       x = "Date",
       y = "Running Mean Temperature in °C") +
  #scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 day") +
  theme_bw()+
  theme(
    text = element_text(size = 16),  # Increase font size for all text
    #   axis.text.y = element_text(size = 16)  # Adjust y-axis text size
  ) 
ggsave(paste("./plots/env/running_mean_Treatment",variable_to_plot,".png", sep="_" ),height=3.5, width=5)

####plot####
ggplot(TOMST.df, aes(x=datetime, y=as.numeric(T1), color=TREATMENT))+
  #geom_point(alpha=0.2, size=0.1)+
  scale_colour_viridis_d(
    option = "D",
    aesthetics = "colour",
    name="Series Number"
  )+
  #ylim(-16,18)+
  geom_point(alpha=0.3, size=0.4)+
  geom_smooth()+
  facet_grid(cols=vars(SUBSITE))+
  ylab("Temperature 1")+
  theme_bw()
ggsave("./plots/env/Temperature1_subsites_treatment.png",height=4, width=5)



ggplot(TOMST.df, aes(x=as.numeric(T1), y=as.numeric(T3), color=TREATMENT))+
    geom_point(alpha=0.2, size=0.1)+
    scale_colour_viridis_d(
      option = "D",
      aesthetics = "colour",
      name="Series Number"
    )+
  geom_smooth(method="lm")+
  theme_bw()
  

ggplot(TOMST.df, aes(x=as.numeric(T1), y=as.numeric(soil_moisture), color=TREATMENT))+
  geom_point(alpha=0.2, size=0.1)+
  scale_colour_viridis_d(
    option = "D",
    aesthetics = "colour",
    name="Series Number"
  )+
  geom_smooth(method="lm")+
  theme_bw()

ggplot(TOMST.df, aes(x=datetime, y=as.numeric(soil_moisture), color=TREATMENT))+
  #geom_point(alpha=0.2, size=0.1)+
  scale_colour_viridis_d(
    option = "D",
    aesthetics = "colour",
    name="Series Number"
  )+
  #ylim(-16,18)+
  geom_point(alpha=0.3, size=0.4)+
  geom_smooth()+
  facet_grid(cols=vars(SUBSITE))+
  ylab("Soil moisture")+
  theme_bw()

lm(data=TOMST.df, T1 ~ TREATMENT)
#df.list[[1]]




# Subset data for OTC and CTL treatments
otc_data <- TOMST.df[TOMST.df$TREATMENT == "OTC", ]
ctl_data <- TOMST.df[TOMST.df$TREATMENT == "CTL", ]



# Subset data for OTC and CTL treatments for the T1 variable
otc_T1 <- TOMST.df$T1[TOMST.df$TREATMENT == "OTC"]
ctl_T1 <- TOMST.df$T1[TOMST.df$TREATMENT == "CTL"]

# Perform t-test for T1 variable
t_test_result <- t.test(otc_T1, ctl_T1)

# Print the t-test result
print(t_test_result)

# Perform t-test for each temperature variable
t_test_results <- lapply(names(otc_data)[grepl("^T[0-9]$", names(otc_data))], function(var_name) {
  t_test_result <- t.test(otc_data[[var_name]], ctl_data[[var_name]])
  return(t_test_result)
})
t_test_results

# Extract p-values from t-test results
p_values <- sapply(t_test_results, function(t_test_result) {
  return(t_test_result$p.value)
})

# Check if any p-value is less than significance level (e.g., 0.05)
significant_indices <- which(p_values < 0.05)
significant_indices

# Print significant variables
if (length(significant_indices) > 0) {
  significant_vars <- names(p_values)[significant_indices]
  cat("Variables with significant differences (p < 0.05):\n")
  cat(significant_vars, sep = "\n")
} else {
  cat("No variable has a significant difference (p >= 0.05).\n")
}

