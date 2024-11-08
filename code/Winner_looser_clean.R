####   Analyse if a species is increasing or decreasing over the last 20 years ####
#     Author: Merle A. Scheiner  , 2024                                       #
#   It used the data created by different files:                              # 
#   Try to create a heatmap of the species over the last 20 years to see which are increasing or decreasing# 
#   It is supposed to merge and clean the 2022 data by reading the xlsx files #
#   The output is a wide dataformat with the species abundance per plot       #
###############################################################################
library(tidyverse)

alllong_abund <- read.csv("alldata_long_abundance.csv")
alllong_abund <- unique(alllong_abund[,-5])
#alllong_abund <- alllong_abund[!(alllong_abund$abundance == 0 | is.na(alllong_abund$abundance)), ]

# Perform multiple replacements in one step
alllong_abund$species <- str_replace_all(alllong_abund$species, 
                                         c("alopecurus.borealis" = "alopecurus.ovatus"
                                         ))
# Split alllong_abund by the "species" column
species_list <- split(alllong_abund, alllong_abund$species)

# Aggregate the data by species, year, treatment, and subsite, summing the abundance
summarized_data <- aggregate(abundance ~ species + YEAR + TREATMENT + SUBSITE, data = alllong_abund, sum, na.rm = TRUE)

# Calculate the difference in abundance between 2003 and 2022 for each species
widersummarized <- summarized_data %>%
  pivot_wider(names_from = YEAR, values_from = abundance) 

difference_data <- widersummarized%>%
  mutate(difference = ifelse(`2003` == 0 & `2022` == 0, NA, `2022` - `2003`)) %>%
  select(species, TREATMENT, SUBSITE, difference)

library(scales)
# Define custom color scale with additional steps
custom_gradient <- scale_fill_gradientn(colors = c("#FF0000", "#DDDDDD", "#66FF00", "darkgreen"),
                                        values = rescale(c(-60, 0, 60, 355)))

ggplot(difference_data[complete.cases(difference_data$difference), ], aes(x = "", y = species, fill = difference)) +
  geom_tile(color = "white") +
  custom_gradient +
 # scale_fill_gradient2(low = "#FF0000", mid = "#DDDDDD", high = "#00FF00", midpoint = 0, name = "Change") +
  theme_minimal() +
  facet_grid(cols = vars(TREATMENT), rows = vars(SUBSITE), scales = "free", space = "free") +
  theme(axis.text.x = element_blank()) +
  labs(x = NULL, y = "Species", title = "Change in Abundance between 2003 and 2022")
getwd()
ggsave("./plots/veg/heatmap_species_2003vs2022.png")
