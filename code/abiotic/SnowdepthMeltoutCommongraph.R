library(ggplot2)
library(lubridate)
library(scales)
library(dplyr)
setwd("~/Documents/Uni/Masterarbeit/Data_Analysis_Endalen")
#source("./code/.R")
plot_data <- merge(result_data,allsnow[,c(1:3,5)], by.x="Site", by.y="PLOT", all=T)

summary_data <- plot_data %>%
  group_by(SUBSITE.y) %>%
  summarize(
    Mean = mean(SNOW_DEPTH_CM),
    SE = sd(SNOW_DEPTH_CM) / sqrt(n())
  )

primary_y_limits <- c(127, 172)  # Custom limits for the primary y-axis
secondary_y_limits <- c(0, 170)  # Custom limits for the secondary y-axis


plot_data$yearday <-  as.numeric(yday(plot_data$FirstZeroDate))
plot <- ggplot(data = plot_data, aes(x = SUBSITE.y, y = yearday)) +
  geom_boxplot() +
  xlab("Habitat")+
  ylab("Date")+
  theme_bw()+
  scale_y_continuous(limits = primary_y_limits)  # Set custom limits for the primary y-axis
plot

# Create a secondary y-axis for SNOW_DEPTH_CM
plot <- plot + geom_col(data=summary_data, aes(y = Mean), fill = "lightblue", position="dodge") +
  stat_summary(
    aes(y = SNOW_DEPTH_CM),
    fun.data = mean_se, 
    geom = "errorbar", 
    width = 0.3,  # Adjust the width for better alignment
    color="darkblue"
  ) +
  scale_y_continuous(
    name = "Date (day of the year)",
#    sec.axis = sec_axis(~., name = "Snow depth [cm]",, breaks = sec_axis_trans(~., breaks = secondary_y_limits))
    sec.axis = sec_axis(trans = ~., breaks = seq(0, 170, by = 50), name = "Snow depth [cm]")
  )+
theme(axis.text.y.right = element_text(color = "blue")  # Change the color of the secondary y-axis labels
  )
plot
getwd()
setwd("/Users/ms/Documents/Uni/Masterarbeit/Data_Analysis_Endalen/plots/env")
ggsave("snow_depth_meltout.png", height=3.5, width=5)


  
  

#plot <- ggplot(data = plot_data, aes(x = SUBSITE.y, y = yearday)) +
#  geom_boxplot() +
#  ylab("Date") +
#  theme_bw()

## Create a secondary y-axis for 'SNOW_DEPTH_CM' using the pre-calculated summary data
#plot <- plot + geom_col(
#  data = summary_data,
#  aes(x = SUBSITE.y, y = Mean, ymin = Mean - SE, ymax = Mean + SE),
#  fill = "darkred",
#  position = position_dodge(width = 0.75)
#) +
#  scale_y_continuous(
#    name = "Date",
#    sec.axis = sec_axis(~., name = "SNOW_DEPTH_CM")
#  )
#
#plot_data$yearday <-  as.numeric(yday(plot_data$FirstZeroDate))
#plot <- ggplot(data = plot_data, aes(x = SUBSITE.y, y = yearday)) +
#  geom_boxplot() +
#  ylab("Date")
#
## Create a secondary y-axis for SNOW_DEPTH_CM
#plot <- plot + geom_col(aes(y = SNOW_DEPTH_CM), fill = "darkred", alpha=0.5) +
#  scale_y_continuous(
#    name = "Date",
#    sec.axis = sec_axis(~., name = "SNOW_DEPTH_CM")
#  )
#
## Print the plot
#print(plot)


