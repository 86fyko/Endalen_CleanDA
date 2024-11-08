#install.packages("tidyverse")
setwd("~/Documents/Uni/Masterarbeit/Data_Analysis_Endalen")
library("tidyverse")
library("readxl")
###load soil moisture data####
Soil_m.df<- read_xlsx("./data/Env/Soil_moisture_all.xlsx")

#names(Soil_m.df) <-  gsub( "X","",names(Soil_m.df)) #replace x in the column name (only run if the rest of the code dosen't work)
Soil_m.df <- as.data.frame(Soil_m.df) #converts tibble to dataframe

#create a long format with one column for the plots, one for date and one for the moisture values
Soil_m.long <- pivot_longer(Soil_m.df, cols = names(Soil_m.df)[-1], names_to="date_p", 
                              values_to="Soil_moisture",values_transform = as.numeric) 
#Create a column for the subsites (extract from the plot name)
Soil_m.long$SUBSITE <- substr(Soil_m.long$PLOT, 1, 5) 
#create a column for the position if it was measured in the upper right(TR) upper left (TL) or bottom right (BR) corner
Soil_m.long$Position <- substr(Soil_m.long$date_p, 12, 13) 
#create one column for the date
Soil_m.long$date <- dmy(substr(Soil_m.long$date_p, 1, 10))

getwd()
treatment <- read.csv("./data/plot_treatments.csv")
Soil_m.long <- merge(Soil_m.long, treatment, all=T)


#create a first plot (each measurement in each corner is treated as replica, therefore: DON'T use this plot)
ggplot(Soil_m.long, aes(x=date, y=Soil_moisture, color=as.character(SUBSITE)))+
  geom_jitter(width=0.4,aes(shape=TREATMENT))+
  geom_smooth(method="lm")+
  ylab("Soil Moisture [vol%]")+
  xlab("Date")+
  scale_colour_viridis_d(
    option = "D",
    aesthetics = "colour",
    name="Habitat"
  )+
  theme_bw()

# create a dataframe with the means over the measurements in each corner
Soil_m.long2 <- pivot_wider(Soil_m.long[,-2], names_from=Position, values_from = Soil_moisture)
Soil_m.long2$mean_moisture <- rowMeans(Soil_m.long2[,-(1:4)], na.rm=T)
getwd()
treatment <- read.csv("./data/plot_treatments.csv")
Soil_m.long2 <- merge(Soil_m.long2, treatment, all=T)

Soil_m.long2 <- subset(Soil_m.long2, TREATMENT %in% c("CTL", "OTC"))


#### Plot the moisture data####

ggplot(Soil_m.long2, aes(x=date, y=mean_moisture, color=as.character(TREATMENT)))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw()+
  facet_grid(rows="SUBSITE")
ggplot(Soil_m.long2, aes(x=SUBSITE, y=mean_moisture, fill=as.factor(TREATMENT)))+
  geom_boxplot()+
  ylab("Soil Moisture [vol%]")+
  xlab("Habitat")+
  scale_fill_viridis_d(
    alpha = 1,
    begin = 0.46,
    end = 0.8,
    option = "D",
    aesthetics = "fill",
    name="Treamtent"
  )+
  theme_bw()
ggsave("./plots/env/soil_mean_subsites.png",  height=3.5, width=5)

ggplot(subset(Soil_m.long2, !(SUBSITE %in% c("BIS-H", "BIS-L"))), aes(x=SUBSITE, y=mean_moisture, fill=as.factor(TREATMENT)))+
  geom_boxplot()+
  ylab("Soil Moisture [vol%]")+
  xlab("Habitat")+
  scale_fill_viridis_d(
    alpha = 1,
    begin = 0.46,
    end = 0.8,
    option = "D",
    aesthetics = "fill",
    name="Treamtent"
  )+
  theme_bw()
ggsave("./plots/env/soil_mean_subsites_n.png",  height=3.5, width=5)




mm <- aov(mean_moisture~ SUBSITE +TREATMENT,Soil_m.long2)
summary(mm)
anova(mm)
tukey_subsite_mm <- emmeans(mm, specs = ~SUBSITE, adjust = "tukey")
tukey_subsite_mm
tukey_treatment_mm <- emmeans(mm, specs = ~TREATMENT, adjust = "tukey")
tukey_treatment_mm

lows_soilm <- subset(Soil_m.long2, SUBSITE %in% c("DRY-L", "CAS-L", "BIS-L"))
mm <- lm(mean_moisture~ SUBSITE +TREATMENT,lows_soilm)
summary(mm)
anova(mm)
tukey_subsite_mm <- emmeans(mm, specs = ~SUBSITE, adjust = "tukey")
tukey_subsite_mm
tukey_treatment_mm <- emmeans(mm, specs = ~TREATMENT, adjust = "tukey")
tukey_treatment_mm


ggplot(subset(Soil_m.long2, date <"2023-06-27"), aes(x=date, y=mean_moisture, color=as.character(SUBSITE)))+
  geom_jitter(alpha=0.8)+
  geom_smooth(method="lm")+
  ylab("Soil Moisture [vol%]")+
  xlab("Date")+
  scale_colour_viridis_d(
    option = "D",
    aesthetics = "colour",
    name="Habitat"
  )+
  theme_bw()
ggplot(subset(Soil_m.long2, date >"2023-06-27"), aes(x=date, y=mean_moisture, color=as.character(SUBSITE)))+
  geom_jitter(alpha=0.8)+
  geom_smooth(method="lm")+
  ylab("Soil Moisture [vol%]")+
  xlab("Date")+
  scale_colour_viridis_d(
    option = "D",
    aesthetics = "colour",
    name="Habitat"
  )+
  theme_bw()
ggplot(Soil_m.long2, aes(x=date, y=mean_moisture, color=as.character(SUBSITE)))+
  geom_jitter(width=0.8,alpha=0.8,aes(shape=TREATMENT))+
  geom_smooth(method="lm")+
  ylab("Soil Moisture [vol%]")+
  xlab("Date")+
  scale_colour_viridis_d(
    option = "D",
    aesthetics = "colour",
    name="Habitat"
  )+
  theme_bw()
ggsave("./plots/env/date_soilM_subsites.png", height=5, width=8)

getwd()#
setwd("/Users/ms/Documents/Uni/Masterarbeit/Data_Analysis_Endalen/data/secondaryData/ENV")
write.csv(Soil_m.long2,"./SoilMoisture_clean.csv" , row.names = FALSE)
