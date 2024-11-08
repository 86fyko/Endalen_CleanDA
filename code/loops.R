#### create anova for vascular species and save them in a list
independenvar <- names(Summary_ENDALEN_SPECIES)[2:4]
biglist <- list()
anovasr <- list()
for(j in 1:length(independenvar)){
  lmspecis <- list()
  anovaresults <- list()
  species <- unique(Summary_ENDALEN_SPECIES$SPECIES_NAME)
  for(i in 1:length(species)){
    subset<- Summary_ENDALEN_SPECIES[Summary_ENDALEN_SPECIES$SPECIES_NAME==species[i],]
    browser()
    if(nrow(unique(subset[,(j+1)]))>1){ 
      lmspecis[[i]]<- lm(data=subset,formula(paste("n", "~", independenvar[j])))# +SPECIES_NAME + YEAR + SUBSITE) ##https://stackoverflow.com/questions/44389159/how-do-i-use-vector-values-as-variables-in-r
      anovaresults[[i]]<- summary(lmspecis[[i]])
      }
    else{lmspecis[[i]]<-NA
    anovaresults[[i]]<-NA}
  }
  names(lmspecis) <- species
  names(anovaresults) <- species
  biglist[[j]] <- lmspecis
  anovasr[[j]]<- anovaresults
}
names(anovasr) <- independenvar
capture.output(anovasr, file = "./models/anova_results.txt")
