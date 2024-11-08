#source("./code/data_wrangling.R")
#source("./code/PreviousYears.R")

datasets[[i]] <-read.csv("./data/secondaryData/allEndalenAbundance.csv")
#### Part 6: Beta Diversity - multivariate statistics         ####

summary[[i]]<-datasets[[i]][1:5]

## all of the data we need is after those three columns so now that we have those elsewhere 
## we can get rid of them
wide_list[[i]]<-datasets[[i]][6:ncol(datasets[[i]])]
head(wide_list[[i]])
head(summary[[i]])
## to calculate the dissimilarities that we talked about we use the function "vegdist()"
## "as.dist" is a distance matrix 
## We can start from our raw in wide format and calculate the bray curtis at the same time
#for(i in (1:length(wide_list))){
as.bray.NMDS<-metaMDS(wide_list[[i]], distance="bray") # you can insert the dryas dataset

## This does not converge so we need to up our max tries 
#as.bray.NMDS<-metaMDS(wide_list[[i]], distance="bray", trymax=100)


ordiplot(as.bray.NMDS)
stressplot(as.bray.NMDS)


## we can display labels on our points 
png(paste("./plots/ordi_spec_",i,".png", sep=""),res=320, width=6, height=3.5, units="in")
ordiplot(as.bray.NMDS, type="t")
dev.off()
### Too much! 

### Add ellipses
### we want to know about how age influences composition 
### so we use the variable "AgeClass" from our summary dataset 
### AgeClass takes our age variable and puts the plots into discrete bins 
### 1=0-4 years, 2=5-9, 3=10-14, 4=15-19, 5=20-24, 6=25-29
png(paste("./plots/ordi_subsites_",i,".png", sep=""),  width=1500, height=1150, units="px", res=250)
ordiplot(as.bray.NMDS)
ordiellipse(as.bray.NMDS,groups=summary[[i]]$SUBSITE,draw="polygon",col="grey90",label=T)
dev.off()
#### These are all overlapping. It looks like there is no strong correlation between variation
### and age class of our forests although young forests may be significantly different than
### old forests 
### lets do an analysis
### We can check whether or not these factors influence our plot using the envfit command
### It's intention was to add environmental factors to community analysis but it works for 
### any variable. It is basically the multivariate version of the lm() command. 

#bray.fit<-envfit(as.bray.NMDS~SUBSITE, data=summary.vascular, perm=999)
#bray.fit

### we can also use the continuous variable "Age" here to see if YEAR is capturing 
### the same information. 
# bray.fit.cont<-envfit(as.bray.NMDS~YEAR, data=summary.vascular, perm=999)
#bray.fit.cont

### then we can add an arrow for our factor to the plot
#png("./plots/ordiplot_spec_end.png",res=320, width=3000, height=1800, units="px")
#ordiplot(as.bray.NMDS, type="t")
#dev.off()
#plot(bray.fit)
#plot(bray.fit.cont)
### Young forests have significantly different composition than young forests.


#### Part 7: Exercises - Beta Diversity and multivariate analysis ####

### 1. Using the BCI data we loaded earlier. Calculate a distance matrix using the 
### Bray-Curtis index. 
### 2. Use an NMDS to examine whether there is significant variation in community composition across the two categories 
### in the BCI.env column called "Stream". 

#as.dist1<-vegdist(BCI, method="bray", binary=FALSE, diag=TRUE, upper=TRUE, na.rm=FALSE)
#as.dist1
### This gives us a vector, we need a matrix 
#as.dist1<-as.matrix(as.dist1)
#head(as.dist1)
#
#BCI.NMDS<-metaMDS(BCI, distance="bray")
#ordiplot(BCI.NMDS)
#stressplot(BCI.NMDS)
#
#bray42.fit<-envfit(BCI.NMDS~Stream, data=BCI.env, perm=999)
#bray42.fit
}
