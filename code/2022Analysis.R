####  Purpose of the code: creates some plots and anovas for the 2022 dataset ####
#     Author: Merle A. Scheiner                                         #
# Its the # part of my code                                             # 
#   It uses the cleaned 2022 data from endalen in a wide dataformat     #
#   The idea was to see if the treatment has an effect on the 2022 data #


setwd("~/Documents/Uni/Masterarbeit/Data_Analysis_Endalen")
library(tidyverse)
#file_simone <- read.csv("./data/secondaryData/d2022PerPlot.csv") #2022 dataset
data2022perPlot <- read.csv("./data/secondaryData/data2022PerPlot.csv") #2022 dataset
treatment <- read.csv("./data/plot_treatments.csv")
data2022 <- merge(data2022perPlot, treatment, by="PLOT", all.x=T)
file_simone <- subset(data2022, TREATMENT %in% c("CTL", "OTC") & SUBSITE %in% c("CAS-L", "DRY-L"))
file_simone$TREATMENT <- as.factor(file_simone$TREATMENT)
file_simone$TREATMENT <- relevel(file_simone$TREATMENT, ref = "CTL")

str(file_simone)

species <- names(file_simone)[-c(1:3, ncol(file_simone), ncol(file_simone)-1)]
file_simone_long <- pivot_longer(file_simone, cols=species, names_to = "species",values_to = "abundance")

####perform the analysis for the old-ITEX####
#file_simone_longer <- subset(file_simone_longer01, TREATMENT==c("OTC", "CTL"))
file_simone_longer<- file_simone_long
ggplot(file_simone_longer, aes(x=species, y=abundance, color = as.factor(SUBSITE)))+
  geom_boxplot() + 
  scale_color_viridis_d()+
  coord_flip()+
  facet_grid(cols=vars(TREATMENT))+
  theme_bw()
ggsave("./plots/veg/boxplot2022.png", width = 20, height = 14, unit= "cm")


ggplot(file_simone_longer, aes(x=species, y=abundance, color = TREATMENT))+
  geom_boxplot() + 
  scale_color_viridis_d()+
  coord_flip()+
  facet_grid(cols=vars(SUBSITE))+
  theme_bw()
ggsave("./plots/veg/boxplot2022treatment.png", width = 14, height = 14, unit= "cm")
m1<- lm(abundance ~ TREATMENT+species , data=file_simone_longer)
summary(m1)
capture.output(summary(m1), file = "./models/anova_results_2022_species_treatment.txt")

ggplot(file_simone_longer, aes(x=species, y=abundance, color = as.factor(Recorder)))+
  geom_boxplot() + 
  scale_color_viridis_d()+
  facet_grid(cols = vars(SUBSITE))+
  coord_flip()+
  theme_bw()

ggplot(file_simone_longer, aes(x=species, y=abundance, color = as.factor(Recorder)))+
  geom_boxplot() + 
  scale_color_viridis_d()+
  #facet_grid(rows = vars(TREATMENT))+
  coord_flip()+
  theme_bw()

file_simone_longer %>%
  group_by(species, Recorder) %>%
  summarize(mean_abundance = mean(abundance, na.rm=T),
            se = sd(abundance) / sqrt(n())) %>%
  ggplot(aes(x = species, y = mean_abundance, fill = as.factor(Recorder))) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = mean_abundance - se, ymax = mean_abundance + se),
    position = position_dodge(width = 0.8),
    #  width = 0.3
  ) +
  scale_fill_viridis_d() +
  coord_flip() +
  theme_bw()

file_simone_longer %>%
  group_by(species, YEAR) %>%
  summarize(mean_abundance = mean(abundance, na.rm=T),
            se = sd(abundance) / sqrt(n())) %>%
  ggplot(aes(x = species, y = mean_abundance)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = mean_abundance - se, ymax = mean_abundance + se),
    position = position_dodge(width = 0.8),
    #  width = 0.3
  ) +
#  scale_fill_viridis_d() +
  coord_flip() +
  theme_bw()
ggsave("./plots/veg/species_2022.png")


#####Analyse the effect of the treatment on the biomass of the plots (summary of the hits)####
head(file_simone)

#file_simone_w <- merge(file_simone, treatment, by="PLOT", all.x=T)

file_simone$plot_biomass <- rowSums(file_simone[,c(4:(length(file_simone)-2))])

ggplot(file_simone, aes(x=PLOT, y=plot_biomass, fill=TREATMENT))+
  geom_col() + 
  scale_fill_viridis_d()+
  #facet_grid(rows = vars(TREATMENT))+
  coord_flip()+
  theme_bw()
ggsave("./plots/veg/Treatment_Subsite_2022_plots.png", width=12, height =10, unit="cm")

#ggplot(file_simone, aes(x=SUBSITE, y=plot_biomass, fill=TREATMENT))+
#  geom_col(position="dodge") + 
#  scale_fill_viridis_d()+
#  coord_flip()+
#  theme_bw()
ggplot(file_simone, aes(x=SUBSITE, y=plot_biomass, fill=TREATMENT))+
  geom_boxplot() + 
  scale_fill_viridis_d()+
  coord_flip()+
  theme_bw()
ggsave("./plots/veg/Treatment_Subsite_2022.png", width=12, height =10, unit="cm")

m_bmt<- lm(plot_biomass ~ TREATMENT, data=file_simone)
m_bmt<- lm(plot_biomass ~ TREATMENT+ SUBSITE, data=file_simone)
summary(m_bmt)

#next step: write a loop to analyse the result for every subsite (actually only DRY-L, CAS-L and BIS-L)
lowsites <- c("DRY-L", "CAS-L")#, "BIS-L")
ctrlotc <- c("CTL", "OTC")
file_simone2 <- subset(file_simone, TREATMENT==ctrlotc)

lmspecis <- list()
anovaresults <- list()
for(i in 1:length(lowsites)){
  lmspecis[[i]]<- lm(plot_biomass ~ TREATMENT, data=subset(file_simone2, SUBSITE==lowsites[i]))
  anovaresults[[i]]<- summary(lmspecis[[i]])
}
names(anovaresults) <- lowsites
capture.output(anovaresults, file = "/Users/ms/Documents/Uni/Masterarbeit/Data_Analysis_Endalen/models/anova_results_treatment_subsite.txt")
getwd()

