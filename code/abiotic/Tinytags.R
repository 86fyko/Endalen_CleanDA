setwd("~/Documents/Uni/Masterarbeit/Data_Analysis_Endalen/data")
treatment <- read.csv("plot_treatments.csv")
setwd("./env/Tiny_tags/data")

library(tidyverse)
library(data.table)
library(lubridate)
#### CONVERT .xls files into .csv####
## does not work##
#library(readxl)
#library(gdata)
#setwd("../Data_Analysis_Endalen/Tiny_tags")
#file.list <- list.files(pattern='*.xls')
#df.list <- lapply(file.list, read_xls) #gdata::read.xls
##df.list <- lapply(file.list, read.xls)
#
#read_xls("./tg985128_data.xls", sheet=1)
#file.exists("tg985128_data.xls")
#format_from_ext("tg985128_data.xls")
#format_from_signature("tg985128_data.xls")
#?read_xls
#gdata::read.xls("./tg954468_data.xlsx", sheet=1)
#
#library(openxlsx)
#my_data <- read_excel("./tg954468_data.xls")
#
#library("rio")
#xls <- dir(pattern = "xlsx")
#created <- mapply(convert, xls, gsub("xlsx", "csv", xls))

####Load .csv files into list####
file.list <- list.files(pattern='*.csv')
df.list <- lapply(file.list[-c(24:25)], read.csv) #gdata::read.xls

seriesnumber <- vector()
for(i in 1:length(df.list)){
  seriesnumber[i] <- df.list[[i]][1,3]
}
names(df.list) <- seriesnumber


TTdata <- df.list
plot_sn <- read.csv("../Tiny_Tag.csv")

plots_with_data<- plot_sn[plot_sn[,1] %in% seriesnumber[-18], ]
#nrow(plots_with_data)
#sort(plot_sn[,1])
#sort(seriesnumber[-18])
#TTdata[[2]] <-  gsub( "°C","0",TTdata[[2]]) # it doesnt work need to loop or something

for(i in 1:(length(TTdata))){
names(TTdata[[i]])[c(3:5)] <- TTdata[[i]][4,c(3:5)] #update the col names for temperature#
#TTdata[[i]]$SN <- TTdata[[i]][1,3] #create a column with the series number
TTdata[[i]] <- TTdata[[i]][-c(1:4),]#delete the metadata information for the rest of analysis
TTdata[[i]]<- lapply(TTdata[[i]], function(x) gsub(" °C", "", x)) %>% bind_rows(.) #this causes troubles
#TTdata[[i]]<- lapply(TTdata[[i]], function(x) gsub(" \xb0C", "", x)) %>% bind_rows(.) #this causes troubles
}


#### fix the date####
#Different date formats
#"15/03/2023 01:00"
#"2023-03-15 01:00:00"
#"2023-03-15T01:00:00Z"
#"15-03-2023 01:00"


for(i in 1:(length(TTdata))){
  #TTdata[[i]]$Time[1] <- NA
  if(TTdata[[i]]$Time[1]=="2023-03-15 01:00:00"|TTdata[[i]]$Time[1]=="2023-03-15T01:00:00Z"){
    TTdata[[i]]$datetime <- as_datetime(TTdata[[i]]$Time)
  }
  if(TTdata[[i]]$Time[1]=="15-03-2023 01:00"|TTdata[[i]]$Time[1]=="15/03/2023 01:00"){
   TTdata[[i]]$datetime <- dmy_hm(TTdata[[i]]$Time) #format time
  }
  else{
    print(paste(i, "error"))
  }
}
# TTdata now contains a "datetime" column with standardized date-time values

for(i in 1:(length(TTdata))){
print(TTdata[[i]]$datetime[1]) # check that all the dates look coorect
}

####remove beginning of unrealistic data####
#985126 was retrieved earlier than the other loggers , 2023-05-24 10:00:00
#view(TTdata$"985126")
#view(filter(TTdata$"985126", as.POSIXct(datetime) < "2023-05-24 11:00:00"))
TTdata$"985126" <- filter(TTdata$"985126", as.POSIXct(datetime) < "2023-05-24 11:00:00")

####find plots that got thetags later####

#view(filter(TTdata[[3]], as.Date(datetime) > "2023-08-10 11:00:00")) # find the end of the dataset
# manual cut-off time 	2023-08-23 18:00:00
####create one long dataframe with all the measurements####
TTdata.df <- bind_rows(TTdata, .id = "Number")

str(TTdata.df)

#### delete the values from times where the data loggers where inside####
#find plots that got the tags later
# 21-03-2023 04:00, ##Time the datalogger reached outside temperature: 22-03-2023 14:00
laterPlots <- TTdata.df %>%
  filter(as.Date(datetime) == "2023-03-21 11:00:00" & Temperature > 0) %>%
  pull(Number) %>%
  unique()

filter(TTdata.df, as.Date(datetime) > "2023-08-10 11:00:00")
  
TTdata.dff <- TTdata.df %>%
  filter(!(datetime < as.POSIXct("2023-03-22 14:00:00") & Number %in% laterPlots) & #removes the early data from the loggers that were deployed later
           datetime > as.POSIXct("2023-03-15 18:00:00")&# removes the start of every logger (the time it wasn't at the experiment)
           datetime < as.POSIXct("2023-06-15 18:00:00"))
#           datetime < as.POSIXct("2023-08-23 18:00:00"))



tp.df1 <- merge(TTdata.dff,plot_sn, all.x=T) #merge the data with the infomration about the plot the logger was in
tp.df <- merge(tp.df1,treatment,by.x="Plot", by.y="PLOT", all.x=T )
tp.df$TREATMENT[tp.df$Plot == "CAS-H logger 2"] <- "CTL"

#### create first plots####
ggplot(TTdata.dff, aes(x=datetime, y=as.numeric(Temperature), color=Number))+
  geom_point(alpha=0.2, size=0.1)+
  scale_colour_viridis_d(
    option = "D",
    aesthetics = "colour",
    name="Series Number"
  )+
  #ylim(-16,18)+
 # xlim("2023-03-01", "2023-04-22")+
  #geom_path(alpha=0.3)+
  #geom_smooth()+
  theme_bw()


tp.df$SUBSITE <- substr(tp.df$Plot, 1, 5)

getwd()
setwd("/Users/ms/Documents/Uni/Masterarbeit/Data_Analysis_Endalen/data/secondaryData/ENV")
write.csv(tp.df, "./TinyTags_clean.csv", row.names=F) # saves the clean dataframe so I can use it for other analysis
setwd("/Users/ms/Documents/Uni/Masterarbeit/Data_Analysis_Endalen/plots/env/")

tp.df$Temperature <- as.numeric(tp.df$Temperature)
tp.dff <- tp.df %>%
  group_by(Number)%>%
  arrange(datetime) %>%
  mutate(RunningMean = zoo::rollmean(!!as.name("Temperature"), k = 3, fill = NA))

#creates runnig mean over average temperature of the subsites
tp.dfm <- tp.df %>%
  group_by(SUBSITE, datetime) %>%
  summarise(MeanTemperature = mean(Temperature, na.rm = TRUE)) %>%
  arrange(SUBSITE, datetime) %>%
  mutate(RunningMean = zoo::rollmean(MeanTemperature, k = 3, fill = NA))


ggplot(tp.df, aes(x=datetime, y=as.numeric(Temperature), color=SUBSITE))+
  geom_point(alpha=0.2, size=0.1)+
  scale_colour_viridis_d(
    option = "D",
    aesthetics = "colour",
    name="Series Number"
  )+
#  ylim(-16,18)+
 # geom_path(alpha=0.3)+
  geom_smooth()+
  theme_bw()+
  theme(
    text = element_text(size = 16),  # Increase font size for all text
    #   axis.text.y = element_text(size = 16)  # Adjust y-axis text size
  ) 
ggsave("temp_time_Subsite.png")

ggplot(tp.dfm, aes(x=datetime, y=RunningMean, color=SUBSITE))+
#  geom_point(alpha=0.2, size=0.1)+
  geom_line(alpha=0.5)+
  scale_colour_viridis_d(
    option = "D",
    aesthetics = "colour",
    name="Habitat"
  )+
  #  ylim(-16,18)+
  # geom_path(alpha=0.3)+
  theme_bw()+
  theme(
    text = element_text(size = 16),  # Increase font size for all text
    #   axis.text.y = element_text(size = 16)  # Adjust y-axis text size
  ) 
ggsave("temp_time_Subsite_mean.png")

tp.df <- subset(tp.df, TREATMENT %in% c("CTL", "OTC", "NA"))
ggplot(tp.df, aes(x=datetime, y=as.numeric(Temperature), color=TREATMENT))+
  geom_point(alpha=0.2, size=0.1)+
  scale_colour_viridis_d(
    option = "D",
    aesthetics = "colour",
    name="Treatment"
 #   name="Series Number"
  )+
 # ylim(-16,18)+
#   geom_path(alpha=0.3)+
  labs(title = "Temperature of TinyTags",
       x = "Date",
       y = "Temperature in °C") +
#  geom_smooth()+
  theme_bw()+
  theme(
    text = element_text(size = 16),  # Increase font size for all text
    #   axis.text.y = element_text(size = 16)  # Adjust y-axis text size
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        size =5 ,  # Adjust size of legend key
        alpha = 0.8 # Adjust alpha for the legend points (dots)
    ))
  )
ggsave("temp_time_treatment.png",height=3.5, width=5)




# Subset data for OTC and CTL treatments
casdry <- subset(tp.df,SUBSITE %in% c("CAS-L", "DRY-L"))%>%
  filter(., as.Date(datetime) < "2023-06-15 11:00:00")

otc_data <- casdry[casdry$TREATMENT == "OTC", ]
ctl_data <- casdry[casdry$TREATMENT == "CTL", ]

#"CAS-L", "DRY-L"
cas_data <- casdry[casdry$SUBSITE == "CAS-L", ]
dry_data <- casdry[casdry$SUBSITE == "DRY-L", ]

# Convert relevant variables to numeric format
otc_data$Temperature <- as.numeric(otc_data$Temperature)
ctl_data$Temperature <- as.numeric(ctl_data$Temperature)

# Check for missing values
any(is.na(otc_data$Temperature))
any(is.na(ctl_data$Temperature))

# Remove missing values, if present
otc_data <- otc_data[!is.na(otc_data$Temperature), ]
ctl_data <- ctl_data[!is.na(ctl_data$Temperature), ]

# Perform t-test for T1 variable
t_test_result <- t.test(otc_data$Temperature, ctl_data$Temperature)
t_test_result <- t.test(cas_data$Temperature, dry_data$Temperature)

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

#creates runnig mean over average temperature of the subsites
tp.dfmt <- tp.df %>%
  group_by(TREATMENT, datetime) %>%
  summarise(MeanTemperature = mean(Temperature, na.rm = TRUE)) %>%
  arrange(TREATMENT, datetime) %>%
  mutate(RunningMean = zoo::rollmean(MeanTemperature, k = 3, fill = NA))

ggplot(tp.dfmt, aes(x=datetime, y=RunningMean, color=TREATMENT))+
  #  geom_point(alpha=0.2, size=0.1)+
  geom_line(alpha=0.8)+
  scale_colour_viridis_d(
    option = "D",
    aesthetics = "colour",
    name="Treatment"
  )+ 
  labs(title = "Running Mean over 3 hours",
          x = "Date",
          y = "Running Mean") +
  #  ylim(-16,18)+
  # geom_path(alpha=0.3)+
  theme_bw()+
  theme(
    text = element_text(size = 16),  # Increase font size for all text
    #   axis.text.y = element_text(size = 16)  # Adjust y-axis text size
  ) 
ggsave("temp_time_treatment_mean.png",height=3.5, width=5)


ggplot(tp.df, aes(x=datetime, y=as.numeric(Temperature), color=Plot))+
  geom_point(alpha=0.2, size=0.1)+
  scale_colour_viridis_d(
    option = "D",
    aesthetics = "colour",
    name="Plot"
  )+
  facet_grid(vars(SUBSITE))+
  # geom_path(alpha=0.3)+
  geom_smooth()+
  theme_bw()+
  theme(
    text = element_text(size = 16),  # Increase font size for all text
    #   axis.text.y = element_text(size = 16)  # Adjust y-axis text size
  ) 


ggplot(subset(TTdata.dff, Number == "954468"), aes(x=datetime, y=as.numeric(Temperature), color=Number))+
  #geom_point(alpha=0.2, size=0.1)+
  scale_colour_viridis_d(
    option = "D",
    aesthetics = "colour",
    name="Series Number"
  )+
  geom_line(alpha=0.3)+
  geom_smooth()+
  theme_bw()+
  theme(
    text = element_text(size = 16),  # Increase font size for all text
    #   axis.text.y = element_text(size = 16)  # Adjust y-axis text size
  ) 

#tag68 <- subset(TTdata.df, Number == "954468")
#subset(tag68, date<"2023-04-15") %>% subset(.,Temperature > 0)

view(TTdata.df[c(70:110),])
#ggplot(TTdata.df, aes(x=Number, y=as.numeric(Temperature), color=Number))+
#  geom_boxplot()



