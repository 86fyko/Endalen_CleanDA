setwd("~/Documents/Uni/Masterarbeit/Data_Analysis_Endalen")

#read required datafiles
allsnow<- read.csv("./data/secondaryData/ENV/all_snowdata.csv")
Summary_ENDALEN_SPECIES <- read.csv("./data/secondaryData/summary_Endalen_Species.csv") #number of hits? calculated fomr PreviousYears.R


snowveg<- merge(Summary_ENDALEN_SPECIES, allsnow,all=T)
str(snowveg)

snowveg_r<- merge(EndalenOld, allsnow,all=T)
str(snowveg_r)

ggplot(snowveg, aes(x=SNOW_DEPTH_CM, y=n, color=SPECIES_NAME))+
  geom_point()

ggplot(snowveg, aes(x=SNOW_DEPTH_CM, y=SPECIES_NAME, size=n))+
  geom_point()


ggplot(snowveg, aes(y=SNOW_DEPTH_CM, x=SPECIES_NAME))+
  geom_boxplot()+
  coord_flip()

snopm <- lm(data=snowveg, SNOW_DEPTH_CM ~ SPECIES_NAME+ TREATMENT)
summary(snopm)

snoptm <- lm(data=snowveg, SNOW_DEPTH_CM ~ SPECIES_NAME)
summary(snoptm)

AIC(snopm)
AIC(snoptm) # slighly lower AIC



####new code####
####load packages####
library(dplyr)

####setwd load data####

setwd("/Users/ms/Documents/Uni/Masterarbeit/Data_Analysis_Endalen/data/")
treatment <- read.csv("./plot_treatments.csv")
dryCAS_LH <- read.csv( "./secondaryData/dryCAS_LH.csv")
setwd("/Users/ms/Documents/Uni/Masterarbeit/Data_Analysis_Endalen/data/secondaryData/ENV")

#read all the different environmental data
TOMST_long <- read.csv( "./TOMST_clean.csv") # saves the clean dataframe so I can use it for other analysis
TinyTags_long <- read.csv("./TinyTags_clean.csv") # saves the clean dataframe so I can use it for other analysis
Soil_moisture_long <- read.csv("./SoilMoisture_clean.csv" )
allsnow<- read.csv("./all_snowdata.csv")
NDVI_long <- read.csv("./NDVI_clean.csv")

### make sure they are all in wide format
#head(TinyTags) #in long format
head(allsnow) #wide format
head(TOMST_long)#in long format
head(NDVI_long)

####prepare data for merging####

#prepare TOMST_logger
#daily means
# Convert the 'datetime' column to a Date object
TOMST_long$datetime <- as.POSIXct(TOMST_long$datetime)
TOMST_long$date <- as.Date(TOMST_long$datetime)

# Group by the 'date' column and calculate daily means
TOMST_daily <- TOMST_long %>%
  group_by(Plot,date) %>%
  summarise(
    mean_T1 = mean(T1),
    mean_T2 = mean(T2),
    mean_T3 = mean(T3),
    mean_soil_moisture = mean(soil_moisture)
  )

# Print the resulting dataframe
print(TOMST_daily)
TOMST_wide <- pivot_wider(TOMST_daily, names_from=date, values_from=names(TOMST_daily)[-c(1:2)]) #add prefix to the dates

#prepare NDVI
NDVI_wide <- pivot_wider(NDVI_long[,-c(4:6)], names_from=date, values_from=mean_NDVI) #add prefix to the dates
NDVI_wide$season <- rowMeans(NDVI_wide[,-(1:3)], na.rm=T)
names(NDVI_wide)[-c(1:3)] <- paste0("NDVI_", names(NDVI_wide)[-c(1:3)])

#prepare Soil moisture
head(Soil_moisture_long) #in long format

Soil_moisture_wide <- pivot_wider(Soil_moisture_long[,-c(5:8)], names_from=date, values_from=mean_moisture) #add prefix to the dates
#create season mean: I actually shouldn't do that because there is a systematical error 
#because in the beginning i couldn't measure all plots because some where still snow covered. # they are discarted
#However, the soil moisture changes over the season therefore this opereation shouln't be done 
#but for the sake of getting a pipeline started I'll do it until i come up with a better solution

# Those are only the moisture measurements where I have values for every plot
Soil_moisture_wide$mean_moisture_season <- rowMeans(Soil_moisture_wide[,-(1:6)], na.rm=F) 

#names(Soil_moisture_wide)[-c(1:3)] <- paste0("moisture_", names(Soil_moisture_wide)[-c(1:3)])

##### Merge dataframes####
dryCAS22<- subset(dryCAS_LH, YEAR=="2022")
envveg<- merge(Soil_moisture_wide[, c(1:3, 12)] ,NDVI_wide[,c(1,5)],by.x="PLOT", by.y="Plot", all=T) %>%
  merge(., allsnow[,c(3,5)], all=T)%>%  merge(.,dryCAS22[,c(2,34)], all=T)

envveg_dC <- unique(subset(envveg, TREATMENT %in% c("CTL", "OTC") & !(SUBSITE %in% c("BIS-L", "BIS-H"))))


snow_NDVI<- merge(NDVI_wide[,c(1,3,5)],allsnow[,1:4],by.y="PLOT", by.x="Plot", all=T) %>% subset(., TREATMENT %in% c("CTL", "OTC") & !(SUBSITE %in% c("BIS-L", "BIS-H")))
names(snow_NDVI)[3] <- "NDVI_JULY"
lm_snowND<- lm(NDVI_JULY ~ SNOW_DEPTH_CM ,data=subset(snow_NDVI, TREATMENT=="CTL" ))
summary(lm_snowND)

ggplot(snow_NDVI, aes(y=NDVI_JULY, x=SNOW_DEPTH_CM, color=TREATMENT))+
  geom_point()+
  geom_smooth(method="lm")+
  xlab("Snow depth in cm")+
  scale_colour_viridis_d(
    option = "G",
    aesthetics = "colour",
    begin = 0,
    end = 0.8,
    name="Treatment"
  )+
  theme_bw()
ggsave("./plots/veg/snow_NDVI.png", height=3.5, width=5)

snow_NDVI_BM<- merge(snow_NDVI, dryCAS22[,c(2,34)], by.y="PLOT", by.x="Plot",all=T)

ggplot(snow_NDVI_BM, aes(y=plot_biomass, x=SNOW_DEPTH_CM, color=TREATMENT))+
  geom_point()+
  geom_smooth(method="lm")+
  ylab("Snow depth in cm")+
  ylab("Total Life Hits")+
  scale_colour_viridis_d(
    option = "G",
    aesthetics = "colour",
    begin = 0,
    end = 0.8,
    name="Treatment"
  )+
  theme_bw()
ggsave("./plots/veg/snow_Biomass_old.png", height=3.5, width=5)

lm_snowBM_t<- lm(plot_biomass ~ SNOW_DEPTH_CM + TREATMENT ,data=subset(snow_NDVI_BM ) )
lm_snowBM<- lm(plot_biomass ~ SNOW_DEPTH_CM ,data=subset(snow_NDVI_BM, TREATMENT=="CTL" ) )
summary(lm_snowBM)
AIC(lm_snowBM_t,lm_snowBM)

setwd("/Users/ms/Documents/Uni/Masterarbeit/Data_Analysis_Endalen")
lm_moistureND<- lm(plot_biomass ~ mean_moisture_season ,data=unique(subset(envveg_dC[,-7], TREATMENT=="CTL" )))
summary(lm_moistureND)

ggplot(unique(envveg_dC[,-7]), aes(y=plot_biomass, x=mean_moisture_season, color=TREATMENT))+
  geom_point()+
  geom_smooth(method="lm")+
  ylab("Snow depth in cm")+
  ylab("Total Life Hits")+
  scale_colour_viridis_d(
    option = "G",
    aesthetics = "colour",
    begin = 0,
    end = 0.8,
    name="Treatment"
  )+
  theme_bw()
ggsave("./plots/veg/moisture_Biomass.png", height=3.5, width=5)

####test for NDVI####
lm_moistureNDVI<- lm(`NDVI_2023-07-26` ~ mean_moisture_season ,data=unique(subset(envveg_dC[,-7], TREATMENT=="CTL" )))
summary(lm_moistureNDVI)

ggplot(unique(envveg_dC[,-7]), aes(y=`NDVI_2023-07-26`, x=mean_moisture_season, color=TREATMENT))+
  geom_point()+
  geom_smooth(method="lm")+
  ylab("Snow depth in cm")+
  ylab("NDVI July")+
  scale_colour_viridis_d(
    option = "G",
    aesthetics = "colour",
    begin = 0,
    end = 0.8,
    name="Treatment"
  )+
  theme_bw()
ggsave("./plots/veg/moisture_NDVI.png", height=3.5, width=5)
