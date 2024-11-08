# Set working directory and read data
setwd("~/Documents/Uni/Masterarbeit/Data_Analysis_Endalen")
sdata <- unique(read.csv("./alldata_long_abundance.csv"))

# Independent variables
independenvar <- c("YEAR", "TREATMENT")

# Subset data
specdata <- sdata[, c(2, 3, 5, 7, 8)]
specdata_as <- specdata[!is.na(specdata$abundance) & specdata$abundance != 0, ]
subsites <- unique(specdata_as$SUBSITE)

# Initialize list to store results
anova_srt <- list()

# Loop over subsites
for (k in 1:length(subsites)) {
  specdatas <- specdata_as[specdata_as$SUBSITE == subsites[k], ]
  anovasr <- list()
  
  # Loop over independent variables
  for (j in 1:length(independenvar)) {
    lmspecis <- list()
    anovaresults <- list()
    spec <- unique(specdatas$species)
    
    # Loop over species
    for (i in 1:length(spec)) {
      subset <- specdatas[specdatas$species == spec[i], ]
      
      if (length(unique(subset[, independenvar[j]])) > 1) { 
        lmspecis[[i]] <- lm(data = subset, formula(paste("abundance", "~", independenvar[j])))
        anovaresults[[i]] <- summary(lmspecis[[i]])
      } else {
        lmspecis[[i]] <- NA
        anovaresults[[i]] <- NA
      }
    }
    
    # Store results
    names(lmspecis) <- spec
    names(anovaresults) <- spec
    anovasr[[j]] <- anovaresults
  }
  
  # Name the results by the independent variables
  names(anovasr) <- independenvar
  anova_srt[[k]] <- anovasr
}

# Name the top-level list by subsites
names(anova_srt) <- subsites

# Save the results to a file
capture.output(anova_srt, file = "./models/anova_results_spec_i.txt")


#model with all effects (as far as present)
#specdata_as <- specdata[!is.na(specdata$abundance) & specdata$abundance != 0, ]
#subsites<- unique(specdata_as$SUBSITE)
##biglist <- list()
#anova_sr <- list()
#for(k in 1: length(subsites)){
#  specdatas <-  specdata_as[specdata_as$SUBSITE==subsites[k],]
#  lmspecis <- list()
#  anovaresults <- list()
#  spec <- unique(specdatas$species)
##  browser()
#  for(i in 1:length(spec)){ 
#   # browser()
#    subset<- specdatas[specdatas$species==spec[i],]
#    ltreat <- length(unique(subset$TREATMENT))
#    print(ltreat)
#    lyear <- length(unique(subset$YEAR))
#    print(lyear)
#    if((ltreat>1)&(lyear>1)){ 
#       lmspecis[[i]][1]<- lm(data=subset,formula(paste("abundance", "~", "TREATMENT * YEAR")))# +SPECIES_NAME + YEAR + SUBSITE) ##https://stackoverflow.com/questions/44389159/how-do-i-use-vector-values-as-variables-in-r
#       anovaresults[[i]][1]<- summary(lmspecis[[i]])
#    }
#   else if((ltreat>1)&(lyear>1)){ 
#      lmspecis[[i]][2]<- lm(data=subset,formula(paste("abundance", "~", "TREATMENT +YEAR")))# +SPECIES_NAME + YEAR + SUBSITE) ##https://stackoverflow.com/questions/44389159/how-do-i-use-vector-values-as-variables-in-r
#      anovaresults[[i]][2]<- summary(lmspecis[[i]])
#    }
#  else if((lyear>1)){ 
#      lmspecis[[i]][3]<- lm(data=subset,formula(paste("abundance", "~", "YEAR")))# +SPECIES_NAME + YEAR + SUBSITE) ##https://stackoverflow.com/questions/44389159/how-do-i-use-vector-values-as-variables-in-r
#      anovaresults[[i]][3]<- summary(lmspecis[[i]])
#    }
#  else if((ltreat>1)){ 
#      lmspecis[[i]][4]<- lm(data=subset,formula(paste("abundance", "~", "TREATMENT")))# +SPECIES_NAME + YEAR + SUBSITE) ##https://stackoverflow.com/questions/44389159/how-do-i-use-vector-values-as-variables-in-r
#      anovaresults[[i]][4]<- summary(lmspecis[[i]])
#    }
#    else{lmspecis[[i]]<-NA
#    anovaresults[[i]]<-NA}
#  }
#
#  names(lmspecis) <- spec
#  names(anovaresults) <- spec
#  #biglist[[k]] <- lmspecis
#  anova_sr[[k]]<- anovaresults
#}
#names(anova_sr) <- subsites
#capture.output(anova_sr, file = "./models/anova_results_spect.txt")
