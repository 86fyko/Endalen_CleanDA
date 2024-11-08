setwd("~/Documents/Uni/Masterarbeit/Data_Analysis_Endalen")
source("./code/data_wrangling.R")


allendalen_ordered <-read.csv("./data/secondaryData/allEndalenAbundance.csv")
allendalen$poaceae
unique(allendalen_ordered$YEAR)
 
getwd()


#create a subset for each year
vasyear <- split(allendalen_ordered, allendalen_ordered$YEAR)



#datasets <- list(allendalen, VEG_wide)
datasets <- vasyear 
#names(datasets) <- c("vascular", "allvegetation")
names(datasets) <- unique(allendalen_ordered$YEAR)

dummy<-specnumber(allendalen_ordered[,-c(1:5)]) #remove non_plants from the dataset!!!!!!!


#### Species Richness####
## we are going to calculate a lot of things from this data so 
## we want to create a simpler summary
summary <- list()
wide_list <- list()
for (i in (1:length(datasets))){
  summary[[i]]<-datasets[[i]][1:5]
  #old names summary.vascular....)
  browser()
  ## all of the data we need is after those three columns so now that we have those elsewhere 
  ## we can get rid of them
  wide_list[[i]]<-datasets[[i]][6:ncol(datasets[[i]])]
  head(wide_list[[i]])
  head(summary[[i]])
  
  summary[[i]]$Species.richness<-specnumber(wide_list[[i]]) 
  ####PLOT SPECIES Richness over time####
  ggplot(summary[[i]],aes(y=Species.richness,x=YEAR, color=as.factor(SUBSITE)))+
    geom_point()+
    geom_smooth(method="lm")+
    scale_x_continuous(breaks=as.numeric(levels(as.factor(summary[[i]]$YEAR))))+
    theme_bw()
  #ggsave(paste("./plots/veg/spec.rich_years_",names(datasets)[i], sep=""), width=7, height=4)
  
  m1 <- lm(data=summary[[i]], Species.richness ~ YEAR + SUBSITE)
  summary(m1)
  m2 <- lm(data=summary[[i]], Species.richness ~ SUBSITE)
  summary(m2)
  
  #does the richness change over the years
  m2 <- lm(data=summary[[i]], Species.richness ~ YEAR) 
  summary(m2) #-> no
  #check for the high and low plots in different models
  mspecrich_h <- summary[[i]][summary[[i]]$SUBSITE %in% c("BIS-H", "CAS-H", "DRY-H"),] %>% 
    lm(data=., Species.richness ~ YEAR) 
  summary(mspecrich_h)
  
  mspecrich_l <- summary[[i]][summary[[i]]$SUBSITE %in% c("BIS-L", "CAS-L", "DRY-L"),] %>% 
    lm(data=., Species.richness ~ YEAR+SUBSITE) # insignificant without the subsite 
  summary(mspecrich_l)
  
  ## This overemphasizes the rare species so we can calculate evenness
  ## in this case the hits correspond to biomass so not sure if method correct
  ## to look for dominance, this is a bit more complicated
  ## first we need the max abundance and total abundance
  summary[[i]]$N.Max<-apply(wide_list[[i]], 1, max)
  summary[[i]]$N.Total<-apply(wide_list[[i]], 1, sum)
  head(summary[[i]])
  
  ## then we can calculate the Berger Parker Dominance Index
  summary[[i]]$Berger.Parker<-summary[[i]]$N.Max/summary[[i]]$N.Total
  head(summary[[i]])
  
  ggplot(summary[[i]],aes(y=Berger.Parker,x=YEAR, color=as.factor(SUBSITE)))+
    geom_point()+
    geom_smooth(method="lm")+
    theme_bw()
  
  ## inverse simpson's index which balances common and rare species 
  ## is easy using the command "diversity()"
  summary[[i]]$Inverse.Simpson<-diversity(wide_list[[i]], index="invsimpson")
  head(summary[[i]])
  
  ggplot(summary[[i]],aes(y=Inverse.Simpson,x=YEAR, color=as.factor(SUBSITE)))+
    geom_point()+
    geom_smooth(method="lm")+
    theme_bw()
  
  
  ##### vector with the different subsets that are to be analysed#####
  datasets <- c("EndalenOld", "vascular.df") 
  
  # plot to check how many hits per species
  ggplot(EndalenOld, aes(x=SPECIES_NAME, fill=as.factor(YEAR)))+
    geom_bar(position="dodge")+
    coord_flip()
  
  ggplot(vascular.df, aes(x=SPECIES_NAME, fill=as.factor(YEAR)))+
    geom_bar(position="dodge")+
    facet_grid(cols=vars(TREATMENT))+
    coord_flip()
  ggsave("./plots/veg/species_treatment_bar.png")
  
  #### compare how the plants change over the years and treatments for each subsite seperately####
  #TODO: add errorbars and calculate fpr each plot and make boxplot out of it
  subsites <- unique(vascular.df$SUBSITE)
  for(i in 1:length(subsites)){
    SUBSET <- subset(vascular.df, SUBSITE==subsites[i])
    ggplot(SUBSET, aes(x=SPECIES_NAME, fill=as.factor(YEAR)))+
      geom_bar(position="dodge")+
      facet_grid(cols=vars(TREATMENT))+
      coord_flip()
    ggsave(paste("./plots/veg/vascualar_year_treatment_", subsites[i], ".pdf", sep=""))
  }
  
  #### functional groups####
  # hits of each functional group: 
  #something is going on in the evergreen shrubs (probably cassiope), it seems to decrease in the control plots
  #todo: statistics for the plot and see for each group whether the differences between the treatments and between the years are significant
  ggplot(vascular.df, aes(x=GFNARROWwalker,fill=as.factor(TREATMENT)))+
    geom_bar(position="dodge")+
    facet_grid(rows=vars(YEAR))+
    coord_flip()
  ggsave("./plots/veg/funtionalgroups_treatment_bar.png")
  
  #### VASCULAR ONLY####
  # group by plot, summarize number of hits per species per plot per year and treatment
  Summary_ENDALEN_SPECIES <-   group_by(vascular.df, PLOT, YEAR,TREATMENT,SUBSITE) %>%  
    count(., SPECIES_NAME)
  vaswide_sum<- pivot_wider(Summary_ENDALEN_SPECIES, values_from=n, names_from = PLOT)
  
  write.csv(Summary_ENDALEN_SPECIES,"./data/secondaryData/summary_Endalen_Species.csv") #number of hits?
  
  Summary_ENDALEN_SPECIES_fungr<- group_by(vascular.df, PLOT, YEAR, SUBSITE, TREATMENT)%>% count(GFNARROWwalker)
  # make.a new barplot per treatment/spescies with means and errorbars

ggplot(Summary_ENDALEN_SPECIES, aes(x=SPECIES_NAME, y=n, color=as.factor(YEAR)))+
  geom_boxplot()+
  coord_flip()+
  facet_grid(cols=vars(TREATMENT))
#ggsave("./plots/veg/funtionalgroups_treatment_bar.png")

# plot to compare the different treatments and see at which subsite which species contributed most to the biomass
ggplot(Summary_ENDALEN_SPECIES, aes(x=SPECIES_NAME, y=SUBSITE, color=as.factor(YEAR)))+
  geom_jitter(aes(size=n), alpha=0.7)+
  coord_flip()+
  facet_grid(cols=vars(TREATMENT))+
  theme_bw()
ggsave("./plots/veg/vascular_subsite_species_year.png", width=50, height=30,unit="cm")

#plot to compare how the different species occur at the different subsites
ggplot(Summary_ENDALEN_SPECIES, aes(x=SPECIES_NAME, y=SUBSITE, color=as.factor(YEAR)))+
  geom_jitter(aes(size=n), alpha=0.7)+
  coord_flip()+
  facet_grid(rows=vars(YEAR),cols=vars(TREATMENT))+
  theme_bw()


#jitterplot to see the changes in the species over time with the treatment as color
ggplot(Summary_ENDALEN_SPECIES, aes(x=SPECIES_NAME, y=SUBSITE, color=as.factor(TREATMENT)))+
  geom_jitter(aes(size=n), alpha=0.7)+
  coord_flip()+
  facet_grid(cols=vars(as.factor(YEAR)))+
  theme_bw()

ggplot(EndalenOld, aes(x=GENUS, fill=as.factor(YEAR)))+
  geom_bar(position="dodge")+
  coord_flip()+
  facet_grid(cols=vars(TREATMENT))

#create a barplot to see the differences in the treatment for each species. 
#Remember to add errorbars and the means of the plots
Summary_ENDALEN_SPECIES_fungr<- group_by(vascular.df, PLOT, YEAR, SUBSITE, TREATMENT)%>% count(GFNARROWwalker)
#names(Summary_ENDALEN_SPECIES)[5] <- "SPECIES_NAME"
ggplot(Summary_ENDALEN_SPECIES, aes(x=SPECIES_NAME, y=n, color= TREATMENT))+
  geom_boxplot()+
  coord_flip()+
  facet_grid(cols=vars(as.factor(YEAR)))+
  theme_bw()

ggplot(Summary_ENDALEN_SPECIES, aes(x=SPECIES_NAME, y=log(n), color= TREATMENT))+
  geom_boxplot()+
  coord_flip()+
  facet_grid(cols=vars(as.factor(SUBSITE)))+
  theme_bw()

ggplot(Summary_ENDALEN_SPECIES, aes(x=SPECIES_NAME, y=n,color=as.factor(YEAR)))+
  geom_boxplot()+
  coord_flip()+
  facet_grid(cols=vars(TREATMENT))+
  theme_bw()
ggsave("./plots/veg/Summary_ENDALEN_SPECIES_species.png")

lm1_dry<- aov(data=subset(Summary_ENDALEN_SPECIES, SPECIES_NAME=="Dryas octopetala"), n~TREATMENT)
summary(lm1_dry)
plot(lm1_dry)

lm1_cas<- aov(data=subset(Summary_ENDALEN_SPECIES, SPECIES_NAME=="Cassiope tetragona"), log(n)~TREATMENT)
summary(lm1_cas)

ggplot(Summary_ENDALEN_SPECIES_fungr, aes(x=GFNARROWwalker, y=log(n), color= TREATMENT))+
  geom_boxplot()+
  coord_flip()+
  facet_grid(cols=vars(as.factor(YEAR)))+
  theme_bw()

ggplot(Summary_ENDALEN_SPECIES_fungr, aes(x=GFNARROWwalker, y=log(n), color= TREATMENT))+
  geom_boxplot()+
  coord_flip()+
  facet_grid(cols=vars(as.factor(SUBSITE)))+
  theme_bw()

for(i in 1:length(unique(Summary_ENDALEN_SPECIES$SUBSITE))){
  ggplot(subset(Summary_ENDALEN_SPECIES, SUBSITE==unique(Summary_ENDALEN_SPECIES$SUBSITE)[i]), aes(x=SPECIES_NAME,y=n, color=TREATMENT))+
    geom_boxplot()+
    coord_flip()+
    facet_grid(cols=vars(as.factor(YEAR)))
  ggsave(paste("./plots/veg/genus_year_treatment_", unique(EndalenOld$SUBSITE)[i], ".pdf", sep=""))
}
}




#### Species richness (vascular) per site per year ####
#unique(PLOT)
plotssubsets <- list()
for (i in 1:length(unique(EndalenOld$PLOT))){
  plotssubsets[[i]] <- subset(EndalenOld, PLOT==unique(EndalenOld$PLOT)[i])
}
unique(EndalenOld)
 

#### STATISTICS####
m1<- lm(data=Summary_ENDALEN_SPECIES, n ~ SPECIES_NAME)
summary(m1)
plot(m1)

m1<- lm(data=Summary_ENDALEN_SPECIES, n ~ TREATMENT)# +SPECIES_NAME + YEAR + SUBSITE)
summary(m1)
#anova_summary(m1)
plot(m1)

#### create anova for vascular species and save them in a list
source("./code/loops.R")

#für manche arten ist das OTC treatment signifikant und das model auch

m1<- lm(data=Summary_ENDALEN_SPECIES[Summary_ENDALEN_SPECIES$SPECIES_NAME=="Dryas octopetala",], n ~ TREATMENT)# +SPECIES_NAME + YEAR + SUBSITE)

m1<- lm(data=Summary_ENDALEN_SPECIES, n ~ TREATMENT + SPECIES_NAME + YEAR + SUBSITE)

summary(m1)
plot(m1)

# save plots as template datafiele
plots<- unique(EndalenOld[,c(2:4, 17)])
#write.csv(plots, "../data/template_endalen_plots.csv")
#}


