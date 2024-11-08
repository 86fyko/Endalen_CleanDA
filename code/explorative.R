#install.packages("tidyverse")
library("tidyverse")

Endalen2015df<- read.csv("./data/veg/CSV2/Endalen_PF_2015.csv")
str(Endalen2015df)
range(Endalen2015df$ABUNDANCE)


ggplot(Endalen2015df, aes(x=SPP))+
  geom_bar()+
  coord_flip()



ggplot(Endalen2015df, aes(x=SPP, color= SUBSITE))+
  geom_bar()+
  coord_flip()


plots_v <- unique(Endalen2015df$PLOT)

plot_subsets_l <- list()
for(i in 1:length(plots_v)){
  plot_subsets_l[[i]] <- subset(Endalen2015df, PLOT == plots_v[i])
}

#subset(Endalen2015df, plot = plots_v[i])

#VASCULAR <- subset(Endalen2015df, plot = plots_v[i])

plot_species <- list() ## keep only vascular plants
plot_diversity <- vector()
for(i in 1:length(plots_v)){
  plot_species[[i]] <- unique(plot_subsets_l[[i]]$SPP)
  plot_diversity[i] <- length(unique(plot_subsets_l[[i]]$SPP))
}
names(plot_diversity) <- plots_v
names(plot_species) <- plots_v

hist(plot_diversity)
‚

