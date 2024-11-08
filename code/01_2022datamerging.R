####  Purpose of the code: Merge the 2022 datasets                         ####
#     Author: Merle A. Scheiner  , 2024                                       #
#   It should be fairly similar to the data_wrangling codes                   # 
#   It is supposed to merge and clean the 2022 data by reading the xlsx files #
#   The output is a wide dataformat with the species abundance per plot       #
#   output files: "./2022data.csv", "./data2022PerPlot.csv" and completeness  #
###############################################################################

#### load packages####
library(tidyverse)
library(viridis)
library(readxl)
library(taxadb)

setwd("~/Documents/Uni/Masterarbeit/Data_Analysis_Endalen")

####load data####
#DRY_low<- read_excel("data/veg/RAW_Excel/Endalen_2022_AL_Dryas.xlsx",sheet = 2)

path <- "./data/veg/RAW_Excel/"
setwd (path)
file.list2022 <- list.files(pattern='*.xlsx') # %>% 
#filelist2022 <- list.files(pattern='*.csv')
file.list2022 <- file.list2022[!grepl("^~\\$", basename(file.list2022))]

veg.list2022 <- lapply(file.list2022[-length(file.list2022)], read_excel,sheet = 2) #gdata::read.xls
#veg.list2022 <- lapply(file.list2022[-length(file.list2022)], read.csv) #gdata::read.xls
#veg.list2022[[length(veg.list2022)+1]] <- read.csv("./ENDALEN_ALLSITES_2003to2015.csv")
meta.list2022 <- lapply(file.list2022, read_excel,sheet = 1) #gdata::read.xls

for(i in 1:(length(veg.list2022))){ #if old data in list:length(veg.list2022)-1)
  veg.list2022[[i]]$YEAR <-  year(veg.list2022[[i]]$DATE)
}


####create one long dataframe with all the measurements####
# Convert all columns to characters in all data frames in veg.list2022
veg.list2022 <- lapply(veg.list2022, function(df) {
  df[] <- lapply(df, as.character)
  return(df)
})
vegdata2022 <- bind_rows(veg.list2022) #its important that all the files have the same structure
meta <- unique(bind_rows(meta.list2022))[-1,] #creates one common dataframe without duplicated rows and delete row with explanation for colnames

meta$SPECIES_NAME <- apply(meta[ , c("GENUS","...7") ] , 1 , paste , collapse = " " )
#### something is wrong over here#####
vegdata2022.df <- merge(unique(vegdata2022), meta[,c("SPP","GFNARROWwalker", "GENUS", "SPECIES_NAME")],by="SPP", all.x=T) #add metadata to data

nrow(vegdata2022.df)==nrow(vegdata2022) #double check that we didn't loose any rows
nrow(vegdata2022.df)
nrow(vegdata2022)
missing_rows <- vegdata2022[!vegdata2022$SPP %in% vegdata2022.df$SPP, ]
which(!vegdata2022$SPP %in% vegdata2022.df$SPP)
unique(vegdata2022$SPP )


#### clean species names####
# Check for missing values in SPECIES_NAME column
missing_species <- which(is.na(vegdata2022.df$SPECIES_NAME))
# Replace missing values with values from the SPP column
vegdata2022.df$SPECIES_NAME[missing_species] <- vegdata2022.df$SPP[missing_species]
#vegdata2022.df$SPECIES_NAME[missing_species] <- vegdata2022.df$SPP[missing_species][missing_species]
unique(vegdata2022.df$SPECIES_NAME)
unique(vegdata2022.df$SPP)

subset(vegdata2022.df, SPP=="dru")

vegdata2022.df$DATE <- as.character(vegdata2022.df$DATE)

vegdata2022.df$SPECIES_NAME[missing_species]
which(is.na(vegdata2022.df$SPECIES_NAME))

subset(vegdata2022.df, is.na(vegdata2022.df$SPECIES_NAME))

# check for species names in general:

unique(vegdata2022.df$SPECIES_NAME)
# Convert to lowercase
vegdata2022.df$SPECIES_NAME <- clean_names(vegdata2022.df$SPECIES_NAME)

# Perform multiple replacements in one step
vegdata2022.df$SPECIES_NAME <- str_replace_all(vegdata2022.df$SPECIES_NAME, 
                                           c("aulocomnium" = "aulacomnium", 
                                             "pecouros" = "alopecurus",
                                             "flavocflavocetraria" ="flavocetraria",
                                             "luzniv" = "luzula nivalis",
                                             "lzucon" = "luzula confusa",
                                             "dapa" = "draba",
                                             "pticil"= "ptilidium ciliare",
                                             "stelaria" = "stellaria",
                                             "alopecurus borealis" = "alopecurus ovatus",
                                             "stere0" ="stereo",
                                             "racumitrium" = "racomitrium",
                                             "racomitirum" = "racomitrium",
                                             "racomi"= "racomitrium"
                                             ))
# Remove numbers from SPECIES_NAME column - check if you really want it
vegdata2022.df$SPECIES_NAME <- gsub("\\d", "", vegdata2022.df$SPECIES_NAME)

unique(vegdata2022.df$SPECIES_NAME)

#subset(vegdata2022.df, SPECIES_NAME =="bis")
vegdata2022.df$SPECIES_NAME <- clean_names(vegdata2022.df$SPECIES_NAME)
unique(vegdata2022.df$SPECIES_NAME)

##### check for data completenes ####
setwd("~/Documents/Uni/Masterarbeit/Data_Analysis_Endalen/data/secondaryData")
#dir.create("./secondaryData", showWarnings = FALSE)
str(vegdata2022.df)
write.csv2(vegdata2022.df, file = "./2022data.csv", row.names = FALSE)

infoPl<- unique(vegdata2022.df[,c(5,9:10,16)])
#view(infoPl)
#view(unique(infoPl[,-3]))
n<- infoPl%>%
  group_by(PLOT, ROW)%>% summarise(Num_Column_Levels = n_distinct(COLUMM))
n_wide <- pivot_wider(n, names_from=ROW, values_from = Num_Column_Levels)  

ggplot(n, aes(y=PLOT, x=ROW, color=as.factor(Num_Column_Levels)))+
  geom_point()+
  theme_bw()
getwd()
#ggsave("./completeness_data_2022.png")

wd1<- getwd()
setwd("~/Documents/Uni/Masterarbeit/Data_Analysis_Endalen/data/")
write.csv(file="./secondaryData/2022completeness.csv",infoPl)
write.csv(file="./secondaryData/2022completeness_summary.csv",n_wide)
setwd(wd1)

# count the number of hits per species and plot
#all species
VEG_Abund <-   group_by(vegdata2022.df, PLOT,SUBSITE,YEAR) %>%  
  count(., SPECIES_NAME)
unique(VEG_Abund$SPECIES_NAME)
#species_info <- unique(vegdata2022.df[,c(8:10)])
#veg_long <- merge(VEG_Abund,species_info)

#vascular plants only
#unique(EndalenOld$GFNARROWwalker)
####Check that all the species have a functional groups####
fungroup2022<- unique(vegdata2022.df[,c("GFNARROWwalker","SPECIES_NAME")])
fungr<- read.csv("funcgroup2022_list.csv")
#write.csv(fungroup2022, "funcgroup2022.csv", row.names=F)

#update the functional groups
# Assuming fungr dataframe contains columns: SPECIES_NAME and GFNARROWwalker

# merge vegdata2022.df with fungr dataframe based on SPECIES_NAME
vegdata2022.dff <- merge(vegdata2022.df, fungr, by = "SPECIES_NAME")

# Update GFNARROWwalker column with values from fungr dataframe
vegdata2022.dff$GFNARROWwalker <- ifelse(is.na(vegdata2022.dff$GFNARROWwalker.y), vegdata2022.dff$GFNARROWwalker.x, vegdata2022.dff$GFNARROWwalker.y)

# Drop unnecessary columns
vegdata2022.dff <- select(vegdata2022.dff, -GFNARROWwalker.x, -GFNARROWwalker.y)

#check that everything is correct
unique(vegdata2022.dff[,c("GFNARROWwalker","SPECIES_NAME")])
nrow(unique(vegdata2022.dff[,c("GFNARROWwalker","SPECIES_NAME")]))
#not_vegetation <- c("ROCK","LITTER","SOIL")
VascularHT <- c("SDECI","GRASS", "FORB","SEVER","SLVASC","RUSH", "SEDGE") # create a vector with the functional groups of the vascular plants
vas2022.df<- vegdata2022.dff[vegdata2022.dff$GFNARROWwalker %in% VascularHT,] # create a subset with data from vascular plants only
#vascular.df <- DRYAS[DRYAS$GFNARROWwalker %in% VascularHT,]

write.csv(vas2022.df, "vascular2022.df.csv")
VASCULAR2022_Abund <-   group_by(vas2022.df,PLOT,SUBSITE,YEAR) %>%  
  count(., SPECIES_NAME)
#species_info <- unique(vascular.df[,c(8:10)])
#vas_long <- merge(VASCULAR2022_Abund,species_info)

# transfer to wide dataset
VASCULAR_wide<- pivot_wider(VASCULAR2022_Abund, values_from=n, names_from = SPECIES_NAME)
VASCULAR_wide[is.na(VASCULAR_wide)]<-0
VASCULAR_wide$`cassiope tetragona` <- VASCULAR_wide$`cassiope tetragona`*3
#correct the cas values
data2022_wide <- VASCULAR_wide
names(data2022_wide)

recorder_PY <- unique(vegdata2022.df[,c(5,14)])#plot,recorder
data2022_wide_R <- merge(VASCULAR_wide,recorder_PY, by="PLOT", all.x=T) #plot,recorder, year


#### save clean dataframe####
getwd()
write.csv(data2022_wide, "./data2022PerPlot.csv", row.names = FALSE)
write.csv(data2022_wide_R, "./data2022PerPlot_rec.csv", row.names = FALSE)
#write.csv(VASCULAR_wide,"./data2022PerPlot.csv",  row.names = FALSE)

names(data2022_wide)
 