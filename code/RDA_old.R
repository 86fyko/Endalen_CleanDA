####  Purpose of the code: run RDA for Dryas in 2022 dataset               ####
#     Author: Merle A. Scheiner  , 2024                                       #
#   needs: DryasPerPlot.csv                                                   # 
#                                                                             #
###############################################################################
####TODO: remember to use the treatments for the RDA (transform them into 1 and 0) ####
#bodenfeuchte unvollständig rausschmeißen
#control vs high
#control vs otc
#ca stadt pca

##### plots über zeit aber nicht RDA####
#repeated anova

#plots über zeit unterschiedliche farbe und treatment in unterschiedlicher form

#modified after:
#Redundancy Analysis (RDA) Tutorial 
#Author: Nicole Regimbal - Just One Bird's Opinion
#Date: June 22, 2023

#Load in necessary libraries
#If these libraries are not yet installed, first use the function install.packages()

###### Load packages######
library(vegan)
library(ggplot2)

####set working directory & load data####
getwd()
setwd("/Users/ms/Documents/Uni/Masterarbeit/Data_Analysis_Endalen")
dir()


#Read in your data - we will call this dataframe 'df'
#DRY_wide<- read.csv("DryasPerPlot.csv", header=T) #vegetation data
allveg <- read.csv("subsetDryasCassiope.csv", header=T)
names(allveg)

env<- read.csv("./data/secondaryData/ENV/allenvironmental.csv", header=T)#environmental data
alldata$PLOT

rownames(env) <- env$PLOT
names(env)

#plots_dryl <-  c("DRY-L1",  "DRY-L10" , "DRY-L2"  ,"DRY-L3" , "DRY-L4" , "DRY-L5" , "DRY-L6" ,"DRY-L7","DRY-L8","DRY-L9")
#ctrl_plots <- c("DRY-H1" ,"DRY-H2", "DRY-H3", "DRY-H4" ,"DRY-H5", "DRY-L1", "DRY-L2" ,"DRY-L5" ,"DRY-L7" ,"DRY-L8")

# Extract the species names from the column names
species_names <- colnames(allveg)[!(colnames(allveg) %in% c("PLOT", "SUBSITE", "YEAR", "SITE", "TREATMENT", "plot_biomass"))]

# Reorder the column names
ordered_column_names <- c("PLOT", "SUBSITE", "YEAR", "SITE",  "TREATMENT", "plot_biomass", species_names)

# Reorder the columns in the dataframe
allveg_reordered <- unique(allveg[, ordered_column_names])

column_names<- names(allveg_reordered)
# Rename columns
# Shorten the names
if(length(column_names[5])>5){
shortened_names <- sapply(column_names[-c(1:4)], function(name) {
  parts <- strsplit(name, "\\.")
  if (length(parts[[1]]) > 1) {
    first_part <- substr(parts[[1]][1], 1, 3)
    second_part <- substr(parts[[1]][2], 1, 3)
    paste0(first_part, second_part)
  } else {
    substr(name, 1, 6)  # Use first 6 characters if no dot is found
  }
})
}

# Check the updated column names
names(allveg_reordered)[-c(1:4)] <- shortened_names[-c(1:4)]


subsite_list <- split(allveg_reordered[,-c(4,6)], allveg_reordered$SUBSITE)


for (j in 1:length(subsite_list)){
#    browser()
    for(i in unique(subsite_list[[j]]$YEAR)){
    subsite_year.df <- subset(subsite_list[[j]], YEAR==i) # %>% subset(., PLOT%in%plots_dryl)%>% #treatment vs control. The second part is only for the old dataset
    #  discard(~all(is.na(.) | . ==""))
    #subsite_year.df <- subset(subsite_list[[j]], YEAR=="2022") %>%subset(., PLOT%in%ctrl_plots)%>% #high vs low
    #  discard(~all(is.na(.) | . ==""))
    #subsite_year.df <- subsite_year.df[,-4] %>% replace(is.na(.), 0) #delete the year from the dataframe and replace all Nas with 0s
    subsite_year.df <- subsite_year.df %>% replace(is.na(.), 0) # replace all Nas with 0s
    
    
    #df <- na.omit(df) #This removes NA values in the dataframe, this is important to avoid errors
    env.dry <- subset(env, PLOT%in%unique(subsite_list[[j]]$PLOT))#%>%discard(~all(is.na(.) | . ==""))
    
    #env.dry <- subset(env, SUBSITE%in%c("DRY-H","DRY-L" ))%>%subset(., TREATMENT=="CTL")%>% 
    #  discard(~all(is.na(.) | . ==""))
    env.dry <- env.dry[, -5] #delete NDVI_season from dataset
    #replace treatment with 0 and 1
    names(env.dry)
    env.dry$TREATMENT<- as.factor(env.dry$TREATMENT)
    levels(env.dry$TREATMENT) <- c(0:3)
    
    env.dry$SUBSITE<- as.factor(env.dry$SUBSITE)
    levels(env.dry$SUBSITE) <- c(0:3)
    
    vegdatapca.df <- as.data.frame(subsite_year.df)
    str(vegdatapca.df)
    rownames(vegdatapca.df) <- vegdatapca.df$PLOT
    head(vegdatapca.df)
    summary(vegdatapca.df)
    
    
    
    ## Model the effect of all environmental variables on fish
    ## community composition
    #spe.rda <- rda(subsite_year.df[-c(1:3)] ~ ., data = env.dry)
    
    #### For abundance data: need for standardization before using it###
    
    #Are you using presence-absence data? Skip this step! 
    #Hellinger transformation on species abundance data 
    #This turns absolute abundance into relative abundance
    #We will call this spec.h
    spec.h <- decostand(vegdatapca.df[-c(1:4)], method = "hellinger")
    #If you are using presence-absence, just take of subset of the species data
    #spec <- subset(df, select = c(15:20))
    
    
    #You don't need this step, but include it if you want to plot the relationships between explanatory variables
    #Normalize and Standardization of environmental factors
    #plot the relationships
    panel.hist <- function(x, ...)
    {
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(usr[1:2], 0, 1.5) )
      h <- hist(x, plot = FALSE)
      breaks <- h$breaks; nB <- length(breaks)
      y <- h$counts; y <- y/max(y)
      rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
    }
    
    pairs(spec.h, panel=panel.smooth, diag.panel=panel.hist)
    
    
    #Are your environmental/explanatory variables on different scales? You need this step! 
    str(env.dry)
    t.env<-decostand(env.dry[,4:5], method="log") #Log transforming the non-standardized variables
    t.env$TREATMENT <- env.dry$TREATMENT
    t.env$SUBSITE <- env.dry$SUBSITE
    
    #t.env <- scale(env[,4:6])
    
    
    #env.stand <- cbind(t.env,data_normalized) #Variables already scaled do not need to be transformed
    
    browser() 
    #Time to do the Redundancy Analyis (RDA)! 
    spec.rda <- rda(spec.h ~ ., t.env)
    summary(spec.rda)
    
    #Let's plot this
    # Opening the graphical device
    png(paste("RDA",names(subsite_list[j]), i, ".png", sep="_"))
    

    
    # Calculate proportion of variance explained by the model
    #variance_explained <- round(varpart(spec.rda, data.frame(t.env, spec.h))$Radj[1] * 100, 1)
    

    
    model <- ordiplot(spec.rda, type = "none", scaling = 2, cex=10, cex.lab=1.25)
    points(spec.rda, col="darkgrey", cex=1)
    points(spec.rda, dis="sp", col="blue")
    text(spec.rda, dis="sp", col="blue")
    text(spec.rda, dis="bp", col="black")

    
    # Add text annotation for variance explained by the model
    #text(x = max(spec.rda$CA$x), y = min(spec.rda$CA$y), labels = paste("Variance Explained:", variance_explained, "%"), pos = 2, cex = 0.8)


   # Closing the graphical device
   dev.off() 
    
    #WAIT - We want to make sure we are using the best model with important terms
    #A step function chooses the best variables to simplify the model
    #spec.rda1 <- step(spec.rda, scope=formula(spec.rda), test="perm")
    #summary(spec.rda1)
    #
    ##Inspect for collinearity to create a simpler model
    ##I am omitting anything with a VIF greater than 20 (This is the usual value used, but adjust to your needs)
    #vif.cca(spec.rda1)
    ##Let's get rid of meadow percent 
    #
    #spec.rda2 <- rda(spec.h ~ Forest.Ratio + Meadow.Ratio + Total.Wetland + Habitat.Area +
    #                   Forest.Percent + Wetland.Percent, env.stand)
    #summary(spec.rda2)
    #
    ##check VIF again - OK!
    #vif.cca(spec.rda2)
    #
    ##You can calculate the adjusted r-squared here
    #RsquareAdj(spec.rda2)
    #
    ##ANOVA
    #anova(spec.rda2, perm.max=1000) #tells you if entire model is significant
    #anova(spec.rda2, by="axis", perm.max=1000) #tells you which axes are significant
    #anova(spec.rda2, by="terms", perm.max=1000) #tells you which terns are significant
    #anova(spec.rda2, by="margin", perm.max=1000) #tells you if the order of the terms is significant
    #
    #summary(spec.rda2)
    #
    ##Let's plot our final, simplified RDA! 
    #simplified_model <- ordiplot(spec.rda2, type = "none", scaling = 2, cex=10, xlab = "RDA1 (22.8%)", ylab = "RDA2 (7.2%)", cex.lab=1.25)
    #points(spec.rda2, col="darkgrey", cex=1)
    #points(spec.rda2, dis="sp", col="blue")
    #text(spec.rda2, dis="sp", col="blue")
    #text(spec.rda2, dis="bp", col="black")
    #
    ##Good luck and happy coding! 
    ##Check out my channel 'Just One Bird's Opinion' on YouTube if you have any questions! 
    #

    #rda(formula,
    #    subsite_list[[j]],
    #    scale = FALSE,
    #    na.action = na.fail,
    #    subset = NULL,
    #    ...
    #)
    }
}

