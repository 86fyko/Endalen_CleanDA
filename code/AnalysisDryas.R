library(dplyr)
setwd("~/Documents/Uni/Masterarbeit/Data_Analysis_Endalen")
Dryas_PerPlot <- read.csv("./data/secondaryData/DryasPerPlot.csv")
allendalen <-read.csv("./data/secondaryData/allEndalenAbundance.csv")
datasets <- list(Dryas_PerPlot,allendalen)
datasets[i]
names(datasets) <- c("Dryas_PerPlot","allendalen")
#Dryas_PerPlot <-data2022_wide
treatment <- read.csv("./data/plot_treatments.csv")

for(i in 1:2){
browser()
Dryas_PerPlot_long <- pivot_longer(Dryas_PerPlot, cols=names(Dryas_PerPlot)[5:27], names_to = "species",values_to = "abundance")
Dryas_PerPlot_longer <- merge(Dryas_PerPlot_long[,-3], treatment, by="PLOT", all.x=T) #remove treatment frim original plot

ggplot(Dryas_PerPlot_longer, aes(x=species, y=abundance, color = as.factor(YEAR)))+
  geom_boxplot() + 
  scale_color_viridis_d()+
  coord_flip()+
  theme_bw()
ggsave("./plots/veg/boxplotDRYAS.png")

ggplot(Dryas_PerPlot_longer, aes(x=species, y=abundance, color = TREATMENT))+
  geom_boxplot() + 
  scale_color_viridis_d()+
  facet_grid(rows = vars(as.factor(YEAR)))+
  coord_flip()+
  theme_bw()
ggplot(Dryas_PerPlot_longer, aes(x=species, y=abundance, color = as.factor(YEAR)))+
  geom_boxplot() + 
  scale_color_viridis_d()+
  facet_grid(rows = vars(TREATMENT))+
  coord_flip()+
  theme_bw()
ggsave("./plots/veg/boxplotDRYAS_treatment.png", height=14)

ggplot(Dryas_PerPlot_longer, aes(x=species, y=abundance, color = as.factor(Recorder)))+
  geom_boxplot() + 
  scale_color_viridis_d()+
  #facet_grid(rows = vars(TREATMENT))+
  coord_flip()+
  theme_bw()

Dryas_PerPlot_longer %>%
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

Dryas_PerPlot_longer %>%
  group_by(species, YEAR) %>%
  summarize(mean_abundance = mean(abundance, na.rm=T),
            se = sd(abundance) / sqrt(n())) %>%
  ggplot(aes(x = species, y = mean_abundance, fill = as.factor(YEAR))) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = mean_abundance - se, ymax = mean_abundance + se),
    position = position_dodge(width = 0.8),
    #  width = 0.3
  ) +
  scale_fill_viridis_d() +
  coord_flip() +
  theme_bw()
ggsave("./plots/veg/species_years.png")

}
