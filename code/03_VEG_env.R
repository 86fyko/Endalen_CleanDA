####  Purpose of the code: correlates vegetation and environment               ####
#     Author: Merle A. Scheiner                                                 #
# This code needs the AnalysisAll code to runfirst to create the alldata_long_abundance.csv file # 
#   It also requires all the environmental variables to be cleaned (use the same env. file as RDA)  #
#   The idea was to see if there are any correlations between the species abundance and the environmenal variables #

# Load in necessary libraries
library(vegan)
library(ggplot2)

# set working directory & load data
#setwd("/Users/ms/Documents/Uni/Masterarbeit/Data_Analysis_Endalen")
datadir <- here("data", "secondaryData")
vegdata_all <- read.csv(paste(datadir, "/alldata_long_abundance.csv", sep=""), header = TRUE) # vegetation data
env <- read.csv(paste(datadir, "/ENV/allenvironmental.csv", sep=""), header = TRUE) # environmental data
rownames(env) <- env$PLOT

#merge dataframes
allend.22.23.df <- merge(unique(subset(vegdata_all, YEAR=="2022")), env, all.x=T)

#create linear models: 
lm_snowveg <- lm(plot_biomass~ SNOW_DEPTH_CM + SUBSITE,unique(allend.22.23.df[, c("plot_biomass", "SNOW_DEPTH_CM","PLOT", "SUBSITE")]))
#lm_snowveg <- lm(abundance~ SNOW_DEPTH_CM + SUBSITE,allend.22.23.df)
summary(lm_snowveg)
AIC(lm_snowveg)

ggplot(allend.22.23.df, aes(x=SNOW_DEPTH_CM, y=plot_biomass, color=TREATMENT, shape=SUBSITE))+
  geom_point()

lm_moistveg <- lm(plot_biomass~TREATMENT + mean_moisture_season + SUBSITE ,unique(allend.22.23.df[, c("plot_biomass", "mean_moisture_season","PLOT", "SUBSITE", "TREATMENT")]))
summary(lm_moistveg)

ggplot(allend.22.23.df, aes(x=mean_moisture_season, y=plot_biomass, color=TREATMENT, shape=SUBSITE))+
  geom_point()+
  geom_smooth(method="lm")




#the environmental variables don't influcence the species but the community biomass
