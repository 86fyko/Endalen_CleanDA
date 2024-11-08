#### Analysis of 2022 Endalen Data - Treatment Effect on Vegetation Abundance and Biomass ####
# Author: Merle A. Scheiner
# Description: This script loads cleaned 2022 data, merges with treatment data, and performs analysis and plotting.

# Libraries
library(tidyverse)
library(here)

# File paths
data_dir <- here("data", "secondaryData")
output_dir <- here("output","plots", "veg")
model_output_dir <- here("output","models")
data_file <- file.path(data_dir, "data2022PerPlot.csv")
treatment_file <- here("data", "plot_treatments.csv")

# Load and preprocess data
data2022perPlot <- read.csv(data_file)
treatment <- read.csv(treatment_file)
data2022 <- merge(data2022perPlot, treatment, by = "PLOT", all.x = TRUE)
file_simone <- subset(data2022, TREATMENT %in% c("CTL", "OTC") & SUBSITE %in% c("CAS-L", "DRY-L"))
file_simone$TREATMENT <- relevel(as.factor(file_simone$TREATMENT), ref = "CTL")

# Transform data to long format for species-specific analysis
species_cols <- names(file_simone)[-c(1:3, ncol(file_simone), ncol(file_simone)-1)]
file_simone_long <- pivot_longer(file_simone, cols = species_cols, names_to = "species", values_to = "abundance")

# Plotting function
save_plot <- function(plot_obj, filename, width = 20, height = 14, units = "cm") {
  ggsave(filename = file.path(output_dir, filename), plot = plot_obj, width = width, height = height, units = units)
}

# Plot abundance per species by treatment and subsite
plot1 <- ggplot(file_simone_long, aes(x = species, y = abundance, color = SUBSITE)) +
  geom_boxplot() +
  scale_color_viridis_d() +
  coord_flip() +
  facet_grid(cols = vars(TREATMENT)) +
  theme_bw()
save_plot(plot1, "boxplot2022.png")

plot2 <- ggplot(file_simone_long, aes(x = species, y = abundance, color = TREATMENT)) +
  geom_boxplot() +
  scale_color_viridis_d() +
  coord_flip() +
  facet_grid(cols = vars(SUBSITE)) +
  theme_bw()
save_plot(plot2, "boxplot2022treatment.png")

# ANOVA: Effect of treatment on abundance by species
m1 <- lm(abundance ~ TREATMENT + species, data = file_simone_long)
summary_output <- capture.output(summary(m1))
writeLines(summary_output, file.path(model_output_dir, "anova_results_2022_species_treatment.txt"))

# Summary plot: Mean abundance by species and recorder
file_simone_long %>%
  group_by(species, Recorder) %>%
  summarize(mean_abundance = mean(abundance, na.rm = TRUE), 
            se = sd(abundance, na.rm = TRUE) / sqrt(n())) %>%
  ggplot(aes(x = species, y = mean_abundance, fill = Recorder)) +
  geom_col(position = position_dodge(0.8)) +
  geom_errorbar(aes(ymin = mean_abundance - se, ymax = mean_abundance + se), 
                position = position_dodge(0.8), width = 0.3) +
  scale_fill_viridis_d() +
  coord_flip() +
  theme_bw()

# Biomass analysis: Summing species abundance per plot
file_simone$plot_biomass <- rowSums(file_simone[, species_cols])

# Plot biomass per plot and subsite
plot_biomass <- ggplot(file_simone, aes(x = PLOT, y = plot_biomass, fill = TREATMENT)) +
  geom_col() +
  scale_fill_viridis_d() +
  coord_flip() +
  theme_bw()
save_plot(plot_biomass, "Treatment_Subsite_2022_plots.png", width = 12, height = 10)

# Plot biomass by subsite and treatment
plot_biomass_subsite <- ggplot(file_simone, aes(x = SUBSITE, y = plot_biomass, fill = TREATMENT)) +
  geom_boxplot() +
  scale_fill_viridis_d() +
  coord_flip() +
  theme_bw()
save_plot(plot_biomass_subsite, "Treatment_Subsite_2022.png", width = 12, height = 10)

# ANOVA: Effect of treatment and subsite on biomass
m_bmt <- lm(plot_biomass ~ TREATMENT + SUBSITE, data = file_simone)
summary_output_bmt <- capture.output(summary(m_bmt))
writeLines(summary_output_bmt, file.path(model_output_dir, "anova_results_treatment_subsite.txt"))

# Looped analysis by subsite
analyze_subsite <- function(subsite) {
  lm_model <- lm(plot_biomass ~ TREATMENT, data = subset(file_simone, SUBSITE == subsite))
  return(summary(lm_model))
}

# Save analysis results for selected subsites
lowsites <- c("DRY-L", "CAS-L")
anova_results <- lapply(lowsites, analyze_subsite)
names(anova_results) <- lowsites
writeLines(capture.output(anova_results), file.path(model_output_dir, "anova_results_treatment_subsite_summary.txt"))

