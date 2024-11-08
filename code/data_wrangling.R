####  This script is supposed to merge the old data with the 2022 data  ####
#     (which is supposed to be cleaned in the 2022datamerging code)     #
#     In a first step the old data is supoosed to be loaded and         #
#########################################################################

#install.packages("tidyverse")
library("tidyverse") 
library("vegan")
library("reshape2")
library("taxadb") #used for cleaning the species names

#setwd("~/Documents/Uni/Masterarbeit/Data_Analysis_Endalen") #set working directoy to project directory

#### import data####
datdir <- here("data","veg", "RAW_Excel")
EndalenOld<- read.csv(paste(datdir,"/ENDALEN_ALLSITES_2003to2015.csv", sep="")) #removes columns that are not relevant for my data analysis
str(EndalenOld)
names(EndalenOld)
Vegetation.df <- EndalenOld[,c(-1,-(6:7), -(10:11), -16)]#removes columns that are not relevant for my data analysis
# not realy relevant but if I want to keep the rows and columns I can not delete column 6:7 and rename them in this step
#names(Vegetation.df)[c(5,6)] <- c("COLUMN", "ROW")


#create output directory for secondary data:
datadir_secondary <- here("data","secondaryData")

####Check that all the species have a functional groups####
unique(EndalenOld[,c("GFNARROWwalker","SPECIES_NAME")])

#### Clean species names####

# check for species names in general:

unique(Vegetation.df$SPECIES_NAME)
# Convert to lowercase
Vegetation.df$SPECIES_NAME <- clean_names(Vegetation.df$SPECIES_NAME)

# Perform multiple replacements in one step
Vegetation.df$SPECIES_NAME <- str_replace_all(Vegetation.df$SPECIES_NAME, 
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
unique(Vegetation.df$SPECIES_NAME)
#### Functional groups####

# Functional groups
species_GFNARROW <- EndalenOld[, c(12,14)] %>% unique(.)

#### Create a subset with Vascular Plants and Horstails only####
unique(EndalenOld$GFNARROWwalker)
not_vegetation <- c("ROCK","LITTER","SOIL")
VascularHT <- c("SDECI","GRASS", "FORB","SEVER","SLVASC","RUSH", "SEDGE") # create a vector with the functional groups of the vascular plants


fungr<- read.csv(paste(datadir_secondary,"/funcgroup2022_list.csv", sep=""))
#write.csv(fungroup2022, "funcgroup2022.csv", row.names=F)

#update the functional groups
# merge vegdata2022.df with fungr dataframe based on SPECIES_NAME
Vegetation.dff <- merge(Vegetation.df, fungr, by = "SPECIES_NAME")

unique(Vegetation.dff[c("SPECIES_NAME", "GFNARROWwalker.y", "GFNARROWwalker.x" )])

unique(Vegetation.dff["SPECIES_NAME"])

# Update GFNARROWwalker column with values from fungr dataframe
Vegetation.dff$GFNARROWwalker <- ifelse(is.na(Vegetation.dff$GFNARROWwalker.y), Vegetation.dff$GFNARROWwalker.x, Vegetation.dff$GFNARROWwalker.y)

# Drop unnecessary columns
Vegetation.dff <- select(Vegetation.dff, -GFNARROWwalker.x, -GFNARROWwalker.y)

#check that everything is correct
unique(Vegetation.dff[,c("GFNARROWwalker","SPECIES_NAME")])

vascular.df<- Vegetation.dff[Vegetation.dff$GFNARROWwalker %in% VascularHT,] # create a subset with data from vascular plants only


####merge old and 2022 data####
# count the number of hits per species and plot
#all species
VEG_Abund <-   group_by(Vegetation.dff, SITE,PLOT,SUBSITE,TREATMENT,YEAR) %>%  
  count(., SPECIES_NAME)
species_info <- unique(Vegetation.dff[,c(8:10)])
veg_long <- merge(VEG_Abund,species_info)

plot_treatment<- unique(VEG_Abund[,c(2,4)])

write_csv(plot_treatment, paste(here("data"),"/plot_treatment.csv", sep=""))



#vascular plants only
VASCULAR_Abund <-   group_by(vascular.df, SITE,PLOT,SUBSITE,TREATMENT,YEAR) %>%  
  count(., SPECIES_NAME)
species_info <- unique(vascular.df[,c(8:10)])
vas_long <- merge(VASCULAR_Abund,species_info)

VASCULAR_Abund$SPECIES_NAME <- clean_names(VASCULAR_Abund$SPECIES_NAME)

# transfor to wide dataset
VASCULAR_wide<- pivot_wider(VASCULAR_Abund, values_from=n, names_from = SPECIES_NAME)
VASCULAR_wide[is.na(VASCULAR_wide)]<-0

VEG_wide<- pivot_wider(VEG_Abund, values_from=n, names_from = SPECIES_NAME)
VEG_wide[is.na(VEG_wide)]<-0


Endalen2022wide<- read.csv(paste(datadir_secondary,"/data2022PerPlot.csv", sep=""))

colnames(VASCULAR_wide) <- gsub(" ", "\\.", colnames(VASCULAR_wide))

allendalen <- merge(VASCULAR_wide,Endalen2022wide[,-ncol(Endalen2022wide)], all = T )


colnames(allendalen)

#add Treatment to allendalen
treatment <- read.csv("./data/plot_treatments.csv")
allendalen <- merge(allendalen[, !(names(allendalen) %in% c("TREATMENT"))], treatment, by="PLOT", all.x=T) #remove treatment from original plot

allendalen$TREATMENT <- as.factor(allendalen$TREATMENT)
allendalen$TREATMENT <- relevel(allendalen$TREATMENT, ref = "CTL")

#allendalen <- allendalen[, !(names(allendalen) %in% c("TREATMENT"))]
allendalen$SUBSITE <-  substr(allendalen$PLOT,1,  5)

# Extract the species names from the column names
species_names <- colnames(allendalen)[!(colnames(allendalen) %in% c("PLOT", "SUBSITE", "YEAR", "SITE", "TREATMENT"))]

# Reorder the column names
ordered_column_names <- c("PLOT", "SUBSITE", "YEAR", "SITE",  "TREATMENT",  species_names)

# Reorder the columns in the dataframe
allendalen_ordered <- unique(allendalen[, ordered_column_names])

colnames(allendalen_ordered)


write.csv(allendalen_ordered,paste(datadir_secondary, "allEndalenAbundance.csv"),
          row.names = FALSE)

