###############################################################################
####   Analyse all the available vegetation recordings in Endalen         ####
#     Author: Merle A. Scheiner  , 2024                                       #
#   It uses data created by different files: data_wranglin.R                  #
#   This script merges and cleans the 2022 data by reading the xlsx files     #
#   The output is a wide dataformat with the species abundance per plot       #
###############################################################################

library(tidyverse)
library(here)  # Load the here package

# Define paths using here() instead of setwd
Dryas_PerPlot <- read.csv(here("data", "secondaryData", "DryasPerPlot.csv"))
allendalen <- read.csv(here("data", "secondaryData", "allEndalenAbundance.csv"))

names(allendalen)
str(allendalen)

# Processing and plotting
browser()  # browse to ensure rows are selected correctly
names(allendalen)
allendalen$plot_biomass <- rowSums(allendalen[,c(6:ncol(allendalen))], na.rm=T)

# Subsets of data
dryCAS <- subset(allendalen, TREATMENT %in% c("CTL", "OTC") & SUBSITE %in% c("CAS-L", "DRY-L"))
dryCAS_LH <- subset(allendalen, TREATMENT %in% c("CTL", "OTC") & !(SUBSITE %in% c("BIS-L", "BIS-H")))

# Write subsets to CSV
write.csv(dryCAS, here("data", "secondaryData", "subsetDryasCassiope.csv"), row.names = FALSE)
write.csv(dryCAS_LH, here("data", "secondaryData", "dryCAS_LH.csv"))

plotdir <-   here("output","plots", "veg")

# Plotting with ggsave using here()
ggplot(dryCAS, aes(x=SUBSITE, y=plot_biomass, color=as.factor(YEAR))) +
  geom_boxplot() + 
  scale_color_viridis_d(name="Year") +
  facet_grid(cols = vars(TREATMENT)) +
  ylab("Total Life Hits") +
  theme_bw()
ggsave(here(plotdir, "Biomass_Treatment_subsites_years_old.png"), width=12, height=8, unit="cm")

ggplot(dryCAS_LH, aes(x=YEAR, y=plot_biomass, color=TREATMENT)) +
  geom_point() + 
  geom_smooth(method="lm") +
  scale_fill_viridis_d(alpha=1, begin=0.46, end=0.8, option="D", aesthetics="color", name="Treatment") +
  facet_wrap(~SUBSITE, nrow=2, ncol=2) +
  ylab("Total Life Hits") +
  xlab("Year") +
  scale_x_continuous(breaks=c(2003, 2009, 2015, 2022)) +
  theme_bw() +
  theme(panel.spacing = unit(1, "lines"))
ggsave(here(plotdir, "Biomass_Treatment_subsites_years.png"), width=12, height=12, unit="cm")

ggplot(dryCAS, aes(x=as.factor(YEAR), y=plot_biomass, fill=TREATMENT)) +
  geom_boxplot() + 
  scale_fill_viridis_d(alpha=1, begin=0.46, end=0.8, option="D", aesthetics="fill", name="Treatment") +
  facet_grid(cols=vars(SUBSITE)) +
  theme_bw()
ggsave(here(plotdir, "Biomass_Treatment_subsites_years2.png"), width=12, height=8, unit="cm")

# Linear models and statistical analyses

# More plotting and analysis as in the original script, continuing to use here() for file paths
write.csv(unique(allendalen.long$species), here("data", "secondaryData", "Species_Endalen.csv"))

# Final plots, again using here() to save output images
ggplot(datalonger, aes(x=species, y=abundance, color=as.factor(YEAR))) +
  geom_boxplot() + 
  scale_color_viridis_d() +
  labs(color = "Year") +
  coord_flip() +
  theme_bw()
ggsave(here(plotdir, paste("boxplot", names(datasets)[i], ".png")), height=14, width=10)

# Additional loop and filtering steps with ggsave for final results
ggsave(here("plotdir", paste("species_years", names(datasets)[i], ".png")), height=14, width=10)
