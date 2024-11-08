# Redundancy Analysis (RDA) Tutorial 
# Author: Nicole Regimbal - Just One Bird's Opinion
# Date: June 22, 2023

# Load in necessary libraries
library(vegan)
library(ggplot2)

# set working directory & load data
setwd("/Users/ms/Documents/Uni/Masterarbeit/Data_Analysis_Endalen")
DRY_wide <- read.csv("DryasPerPlot.csv", header = TRUE) # vegetation data
env <- read.csv("./data/secondaryData/ENV/allenvironmental.csv", header = TRUE) # environmental data
rownames(env) <- env$PLOT

plots_dryl <- c("DRY-L1", "DRY-L10", "DRY-L2", "DRY-L3", "DRY-L4", "DRY-L5", "DRY-L6", "DRY-L7", "DRY-L8", "DRY-L9")
ctrl_plots <- c("DRY-H1", "DRY-H2", "DRY-H3", "DRY-H4", "DRY-H5", "DRY-L1", "DRY-L2", "DRY-L5", "DRY-L7", "DRY-L8")

for (i in unique(DRY_wide$YEAR)) {
  DRY_2022 <- subset(DRY_wide, YEAR == i) %>% subset(., PLOT %in% plots_dryl) %>% discard(~all(is.na(.) | . == ""))
  DRY_2022 <- DRY_2022 %>% replace(is.na(.), 0)
  
  env.dry <- subset(env, PLOT %in% plots_dryl)
  env.dry <- env.dry[, -5] # delete NDVI_season from dataset
  
  # Replace treatment with 0 and 1
  env.dry$TREATMENT <- as.factor(env.dry$TREATMENT)
  levels(env.dry$TREATMENT) <- c(0:3)
  env.dry$SUBSITE <- as.factor(env.dry$SUBSITE)
  levels(env.dry$SUBSITE) <- c(0:3)
  
  # Hellinger transformation on species abundance data
  spec.h <- decostand(DRY_2022[-c(1:3)], method = "hellinger")
  
  # Normalize and Standardization of environmental factors
  t.env <- decostand(env.dry[, 4:5], method = "log")
  t.env$TREATMENT <- env.dry$TREATMENT
  t.env$SUBSITE <- env.dry$SUBSITE
  
  # Redundancy Analysis (RDA)
  spec.rda <- rda(spec.h ~ ., t.env)
  summary(spec.rda)
  browser()
  # Calculate proportion of variance explained by the model
  variance_explained <- round(varpart(spec.rda, data.frame(t.env, spec.h))$Radj[1] * 100, 1)
  
  # Plotting
  png(paste("RDA", i, ".png"))
  
  model <- ordiplot(spec.rda, type = "none", scaling = 2, cex = 10, cex.lab = 1.25)
  points(spec.rda, col = "darkgrey", cex = 1)
  points(spec.rda, dis = "sp", col = "blue")
  text(spec.rda, dis = "sp", col = "blue")
  text(spec.rda, dis = "bp", col = "black")
  
  # Add text annotation for variance explained by the model
  text(x = max(spec.rda$CA$x), y = min(spec.rda$CA$y), labels = paste("Variance Explained:", variance_explained, "%"), pos = 2, cex = 0.8)
  
  dev.off() 
}

