####  This script is supposed to merge and clean all the dryas data.        ####
#     It should be replaced by data _ wrangling                                 #
#     In this case it does not need to be cleaned with the 2022datamerging code #
#     but it would be good to adjust it so it doesnt exist double               #
#     It depends on the data_wrangling code because it needs the treatment.     #
#     I need to double check how i can merge this with the data wrangling code  #
#     maybe I can just completly ignore the script                              #
#     Output: "Endalen_dryas_allyears.csv", "DryasPerPlot.csv", DryasPerPlot.csv#
#################################################################################
#### This First part is covered by data_wrangling.R an therefore should not be run!!!####
#### load packages####
library(tidyverse)
library(viridis)
library(taxadb)

####load data####
#2022 data
datadir <- here("data", "veg", "CSV2")
DRYAS_PI<- read.csv(paste(datadir,"/Endalen_2022_AL_Dryas.csv", sep=""))
DRYAS_M <- read.csv(paste(datadir,"/Endalen_2022_MS_Dryas.csv", sep=""))
DRY_meta <- read.csv(paste(datadir,"/Endalen_2022_AL_Dryas_Meta.csv",sep=""))
DRY_meta <- DRY_meta[-1,] #delete row with explanation for colnames

str(DRY_meta)
x1 <- merge(DRYAS_PI[,c(1:15)], DRYAS_M, all=T)
x2 <- rbind(DRYAS_PI[,c(1:15)], DRYAS_M)

head(x1)
head(x2)
nrow(x1)
nrow(x2)
which(x1!=x2)

levels(as.factor(x1$PLOT))


#create col for the whole species name
DRY_meta$SPECIES_NAME <- apply( DRY_meta[ , c("GENUS","X") ] , 1 , paste , collapse = " " )

DRYAS <- merge(x1, DRY_meta[,c("SPP","GFNARROWwalker", "GENUS", "SPECIES_NAME")], all.x=T) #add metadata to data
nrow(DRYAS)==nrow(x1) #double check that we didn't loose any rows
names(DRYAS)[10] <- "COLUMN"


DRYAS$YEAR <- 2022

DRYAS$SPECIES_NAME <- gsub("Equisetum scirpoides " ,"Equisetum scirpoides",DRYAS$SPECIES_NAME )
#check species names
unique(DRYAS$SPECIES_NAME)



# check for species names in general:
unique(DRYAS$SPECIES_NAME)
# Convert to lowercase
DRYAS$SPECIES_NAME <- clean_names(DRYAS$SPECIES_NAME)

# Perform multiple replacements in one step
DRYAS$SPECIES_NAME <- str_replace_all(DRYAS$SPECIES_NAME, 
                                           c("Aulocomnium" = "aulacomnium", 
                                             "pecouros" = "alopecurus",
                                             "flavocflavocetraria" ="flavocetraria",
                                             "luzniv" = "luzula nivalis",
                                             "lzucon" = "luzula confusa",
                                             "dapa" = "draba",
                                             "pticil"= "ptilidium ciliare",
                                             "Alopecurus borealis"="Alopecurus ovatus"
                                           ))
unique(DRYAS$SPECIES_NAME)

#### This part is similar to the data_wrangling.R but not checked####
#### merge with dryas data from previous years####

## import old data
EndalenOld<- read.csv(paste(here("data","veg","RAW_Excel"),"/ENDALEN_ALLSITES_2003to2015.csv", sep=""))
str(EndalenOld)
names(EndalenOld)
EndalenOld$SPECIES_NAME <- clean_names(EndalenOld$SPECIES_NAME)
Vegetation.df <- EndalenOld[,c(-1, -(10:11), -16)]
names(Vegetation.df)[c(5,6)] <- c("COLUMN", "ROW")
names(Vegetation.df)

# Functional groups
species_GFNARROW <- EndalenOld[, c(12,14)] %>% unique(.)
## Create a subset with Vascular Plants and Horstails only
#unique(EndalenOld$GFNARROWwalker)
#not_vegetation <- c("ROCK","LITTER","SOIL")
VascularHT <- c("SDECI","GRASS", "FORB","SEVER","SLVASC","RUSH", "SEDGE") # create a vector with the functional groups of the vascular plants
vascular.df<- Vegetation.df[Vegetation.df$GFNARROWwalker %in% VascularHT,] # create a subset with data from vascular plants only
DRYAS_VASC <- DRYAS[DRYAS$GFNARROWwalker %in% VascularHT,]

#subset the old dataset for vascular plants only 
DRYAS_old <- subset(vascular.df, SUBSITE==c("DRY-H","DRY-L"))

####merge all the years####
DRYAS.df <- merge(DRYAS_old, DRYAS_VASC[,-c(11:15)], all=T)

# Check species names
unique(DRYAS.df$SPECIES_NAME)
# Create a species translation matrix

DRYAS.df$SPECIES_NAME <- clean_names(DRYAS.df$SPECIES_NAME)
DRYAS.df$SPECIES_NAME <- str_replace_all(DRYAS.df$SPECIES_NAME, 
                                      c("Aulocomnium" = "aulacomnium", 
                                        "pecouros" = "alopecurus",
                                        "flavocflavocetraria" ="flavocetraria",
                                        "luzniv" = "luzula nivalis",
                                        "lzucon" = "luzula confusa",
                                        "dapa" = "draba",
                                        "pticil"= "ptilidium ciliare",
                                        "alopecurus borealis"="alopecurus ovatus",
                                        "luzula arctica" = "luzula nivalis"
                                      ))

# Define a function to replace species names
#replace_species <- function(name) {
#  for (i in seq_along(species_translation$original)) {
#    name <- gsub(species_translation$original[i], species_translation$replacement[i], name, ignore.case = F)
#  }
#  return(name)
#}

# Use sapply to replace species names in the SPECIES_NAME column
#DRYAS.df$SPECIES_NAME <- sapply(DRYAS.df$SPECIES_NAME, replace_species)
# Check the updated species names
unique(DRYAS.df$SPECIES_NAME)

write.csv(DRYAS.df, paste(here("data", "veg", "secondary"),"/Endalen_dryas_allyears.csv", sep=""))

#subset: original experiment
DRYAS.df$plnr <-  as.numeric(substring(DRYAS.df$PLOT,  6))
OE_dry <- DRYAS.df[DRYAS.df$plnr<11,]
#one plot just to see how many hits we have in total
ggplot(DRYAS.df, aes(x=SPECIES_NAME,fill=as.factor(YEAR)))+
  geom_bar(position="dodge")+
  scale_fill_viridis_d()+
  coord_flip()+
  theme_bw()

ggplot(OE_dry, aes(x=SPECIES_NAME,fill=as.factor(YEAR)))+
  geom_bar(position="dodge")+
  #scale_y_log10()+
  scale_fill_viridis_d()+
  coord_flip()+
 # facet_grid(rows=vars(TRATMENT))+
  theme_bw()

#hist(OE_dry$SPECIES_NAME)

ggplot(DRYAS.df, aes(x=PLOT,fill=as.factor(YEAR)))+
  geom_bar(position="dodge")+
  scale_fill_viridis_d()+
  coord_flip()+
  theme_bw()


treatments<- read.csv(paste(here("data"),"/plot_treatments.csv", sep=""))
unique(DRYAS.df[,c("TREATMENT","PLOT")])%>%na.omit()
merge(DRYAS.df, treatments)

#DRYASsub <- subset(DRYAS.df, PLOT 
plotdir <- here("output", "plots", "veg")

ggplot(DRYAS.df, aes(x=PLOT,fill=as.factor(YEAR)))+
  geom_bar(position="dodge")+
  scale_fill_viridis_d()+
  coord_flip()+
  theme_bw()

ggplot(DRYAS.df, aes(x=SPECIES_NAME,fill=as.factor(YEAR)))+
  geom_bar(position="dodge")+
  scale_fill_viridis_d()+
  coord_flip()+
  theme_bw()
ggsave(paste(plotdir, "DRYAS_years.png", sep=""), height=3.5, width=5)


##install.packages("tidyverse")
library("tidyverse")
library("vegan")
library("reshape2")

#### import data####
DRYAS.df <- read.csv(paste(here("data","veg","secondary"),"/Endalen_dryas_allyears.csv",sep=""))
DRYAS.all<- DRYAS.df
str(DRYAS.all)
names(DRYAS.all)
Vegetation.df <- DRYAS.all[,c(-1,-(6:7), -(10:11), -16)]  #why those columns

#### Functional groups####
species_GFNARROW <- DRYAS.all[, c(10,12)] %>% unique(.) #why those columns

#### Create a subset with Vascular Plants and Horstails only####
unique(DRYAS.all$GFNARROWwalker)
not_vegetation <- c("ROCK","LITTER","SOIL")
VascularHT <- c("SDECI","GRASS", "FORB","SEVER","SLVASC","RUSH", "SEDGE") # create a vector with the functional groups of the vascular plants
vascular.df<- Vegetation.df[Vegetation.df$GFNARROWwalker %in% VascularHT,] # create a subset with data from vascular plants only



# count the number of hits per species and plot
#all species
VEG_Abund <-   group_by(DRYAS.all, PLOT,SUBSITE,TREATMENT,YEAR) %>%  
  count(., SPECIES_NAME)
species_info <- unique(DRYAS.all[,c(8:10)])
veg_long <- merge(VEG_Abund,species_info)

#vascular plants only
VASCULAR_Abund <-   group_by(vascular.df,PLOT,SUBSITE,TREATMENT,YEAR) %>%  
  count(., SPECIES_NAME)
species_info <- unique(vascular.df[,c(8:10)])
vas_long <- merge(VASCULAR_Abund,species_info)

dirsecondary <- here("data", "secondaryData")
# transfer to wide dataset
DRY_wide<- pivot_wider(VEG_Abund, values_from=n, names_from = SPECIES_NAME)
#DRY_wide[is.na(DRY_wide)]<-0
write.csv(DRY_wide, here(dirsecondary,"DryasPerPlot.csv"), row.names = FALSE)

recorder_PY <- unique(DRYAS_VASC[,c(5,14,19)])#plot,recorder, year
file_simone <- merge(DRY_wide,recorder_PY, by=c("PLOT", "YEAR"), all.x=T) #plot,recorder, year
nrow(file_simone) == nrow(unique(file_simone))
write.csv(file_simone, here(dirsecondary, "DryasPerPlot_recorder.csv")
          , row.names = FALSE)
