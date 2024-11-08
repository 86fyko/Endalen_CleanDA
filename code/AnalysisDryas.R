# Load required libraries
library(dplyr)
library(ggplot2)
library(viridis)
library(tidyr)
library(here)

# Define paths using `here()`
data_dir <- here("data", "secondaryData")
plot_dir <- here("output","plots", "veg")

# Load data files using `here()`
Dryas_PerPlot <- read.csv(here(data_dir, "DryasPerPlot.csv"))
allendalen <- read.csv(here(data_dir, "allEndalenAbundance.csv"))
treatment <- read.csv(here("data", "plot_treatments.csv"))

# Define datasets in a list
datasets <- list(Dryas_PerPlot = Dryas_PerPlot, allendalen = allendalen)

# Loop through each dataset in `datasets`
for (dataset_name in names(datasets)) {
  
  # Get the dataset
  data <- datasets[[dataset_name]]
  
  # Convert to long format based on column names with dots
  data_long <- pivot_longer(
    data, 
    cols = grep("\\.", names(data), value = TRUE), 
    names_to = "species", 
    values_to = "abundance"
  )
  
  # Merge with treatment data
  data_long <- merge(data_long[,-3], treatment, by = "PLOT", all.x = TRUE)
  
  # Plot abundance by species across years
  ggplot(data_long, aes(x = species, y = abundance, color = as.factor(YEAR))) +
    geom_boxplot() +
    scale_color_viridis_d() +
    coord_flip() +
    theme_bw() +
    ggtitle(paste("Abundance by Species (Year) -", dataset_name))
  ggsave(here(plot_dir, paste0("boxplot_", dataset_name, "_year.png")))
  
  # Plot abundance by species with treatment as a factor, faceted by year
  ggplot(data_long, aes(x = species, y = abundance, color = TREATMENT)) +
    geom_boxplot() +
    scale_color_viridis_d() +
    facet_grid(rows = vars(as.factor(YEAR))) +
    coord_flip() +
    theme_bw() +
    ggtitle(paste("Abundance by Species (Treatment x Year) -", dataset_name))
  ggsave(here(plot_dir, paste0("boxplot_", dataset_name, "_treatment_year.png")), height = 14)
  
  # Plot abundance by species and recorder
  ggplot(data_long, aes(x = species, y = abundance, color = as.factor(Recorder))) +
    geom_boxplot() +
    coord_flip() +
    theme_bw() +
    ggtitle(paste("Abundance by Species (Recorder) -", dataset_name))
  ggsave(here(plot_dir, paste0("boxplot_", dataset_name, "_recorder.png")))
  
  # Mean abundance by species and recorder with error bars
  data_long %>%
    group_by(species, Recorder) %>%
    summarize(
      mean_abundance = mean(abundance, na.rm = TRUE),
      se = sd(abundance, na.rm = TRUE) / sqrt(n())
    ) %>%
    ggplot(aes(x = species, y = mean_abundance, fill = as.factor(Recorder))) +
    geom_col(position = position_dodge(width = 0.8)) +
    geom_errorbar(
      aes(ymin = mean_abundance - se, ymax = mean_abundance + se),
      position = position_dodge(width = 0.8)
    ) +
    scale_fill_viridis_d() +
    coord_flip() +
    theme_bw() +
    ggtitle(paste("Mean Abundance by Species (Recorder) -", dataset_name))
  
  # Mean abundance by species and year with error bars
  data_long %>%
    group_by(species, YEAR) %>%
    summarize(
      mean_abundance = mean(abundance, na.rm = TRUE),
      se = sd(abundance, na.rm = TRUE) / sqrt(n())
    ) %>%
    ggplot(aes(x = species, y = mean_abundance, fill = as.factor(YEAR))) +
    geom_col(position = position_dodge(width = 0.8)) +
    geom_errorbar(
      aes(ymin = mean_abundance - se, ymax = mean_abundance + se),
      position = position_dodge(width = 0.8)
    ) +
    scale_fill_viridis_d() +
    coord_flip() +
    theme_bw() +
    ggtitle(paste("Mean Abundance by Species (Year) -", dataset_name))
  ggsave(here(plot_dir, paste0("species_", dataset_name, "_year.png")))
}
