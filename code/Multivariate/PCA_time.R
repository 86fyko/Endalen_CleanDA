library(tidyverse)
library(viridis)
# Load required libraries
library(ggplot2)  # for visualization
library(tidyr)    # for data manipulation
library(dplyr)    # for data manipulation
#library(prcomp)   # for PCA
library(ggalt)  # for geom_encircle

setwd("~/Documents/Uni/Masterarbeit/Data_Analysis_Endalen")
####Run PCA for the whole dataset####
#VASCULAR_wide <- write.csv("/Users/ms/Documents/Uni/Masterarbeit/Data_Analysis_Endalen/data/secondaryData/data2022PerPlot.csv", row.names = FALSE)
DRY_wide<- read.csv("DryasPerPlot.csv")
plots_dryl <-  c("DRY-L1",  "DRY-L10" , "DRY-L2"  ,"DRY-L3" , "DRY-L4" , "DRY-L5" , "DRY-L6" ,"DRY-L7","DRY-L8","DRY-L9")

DRY <- subset(unique(DRY_wide), PLOT%in%plots_dryl)%>% #subset(., SUBSITE=="DRY-L")%>%
  discard(~all(is.na(.) | . ==""))

#DRY <- DRY %>% replace(is.na(.), 0)

str(DRY)
vegdatapca.df <- as.data.frame(DRY)
str(vegdatapca.df)
rownames(vegdatapca.df) <- paste(vegdatapca.df$PLOT, vegdatapca.df$YEAR, sep="_")
head(vegdatapca.df)
summary(vegdatapca.df)

# It should have columns for species and a column for the year.

colSums(is.na(vegdatapca.df))
vegdatapca.df <- vegdatapca.df %>% replace(is.na(.), 0)
# If your data is in wide format (one column per species), you may need to convert it to long format.
# Example assuming your species columns are named species1, species2, etc.

# Now, 'long_data' should have columns: 'year', 'species', and 'abundance'.

# Perform PCA
pca_result <- prcomp(vegdatapca.df[,-c(1:4)], scale. = TRUE)  # Exclude 'year' from PCA

# Summary of PCA
summary(pca_result)

# Scree plot to visualize variance explained by each PC
screeplot(pca_result, type = "line")

png("./plots/multivariate/biplot_allyears.png", width = 800, height = 600)  # Set the dimensions as needed
# Biplot to visualize the relationship between samples and species
biplot(pca_result)
dev.off()

# Plot PC scores over time
scores <- as.data.frame(pca_result$x)  # Extract PC scores
scores$year <- as.factor(vegdatapca.df$YEAR)           # Add the 'year' column
scores$PLOT <- vegdatapca.df$PLOT 
scores <- merge(scores, treatment, by="PLOT", all.x=T)

# Extract loadings (contributions of each species to each PC)
loadings <- as.data.frame(pca_result$rotation)

# Plot using ggplot2
ggplot(scores, aes(x = PC1, y = PC2, color = factor(year))) +
  geom_point() +
  labs(title = "PCA of Vegetation Data Over Time",
       x = "PC1",
       y = "PC2",
       color = "Year")

loadings$species <- rownames(loadings)
loadings %>% select(species, PC1, PC2)

# Calculate the explained variation for each PC
explained_var <- pca_result$sdev^2 / sum(pca_result$sdev^2) * 100

ggplot(scores, aes(x = PC1, y = PC2)) +
  geom_segment(data = loadings %>% select(species, PC1, PC2),
               aes(x = 0, y = 0, xend = PC1, yend = PC2, color = species),
               arrow = arrow(length = unit(0.2, "cm")),
               linewidth = 0.5) +  # Add arrows for species contributions
  scale_color_viridis(discrete = TRUE, option = "viridis", name = "Species") +  # Use viridis color scale for species
  theme_bw()+
  geom_encircle(aes(x = PC1, y = PC2, group = factor(TREATMENT),linetype = factor(TREATMENT))) +  #, alpha = 0.2 Add circles around points with the same year
  geom_point(aes(shape=year)) +
  geom_text(aes(label = PLOT), size = 3, vjust = -0.5, color = "black") +  # Add plot_id labels
  scale_shape_manual(values = c(16, 17, 18, 19), name = "Year") +  # Use different shapes for different years
  #  geom_encircle(aes(x = PC1, y = PC2, group = factor(TREATMENT),fill = factor(year))) +  #, alpha = 0.2 Add circles around points with the same year
  labs(title = "PCA of Vegetation Data Over Time",
       x = paste("PC1: ", round(explained_var[1], 2), "%"),
       y = paste("PC2: ", round(explained_var[2], 2), "%"),
       color = "Year")
getwd()
ggsave("./plots/multivariate/pca_years.png", height=10, width= 15)


##### rerun the same code just for 2022####
DRY_wide<- read.csv("DryasPerPlot.csv")
plots_dryl <-  c("DRY-L1",  "DRY-L10" , "DRY-L2"  ,"DRY-L3" , "DRY-L4" , "DRY-L5" , "DRY-L6" ,"DRY-L7","DRY-L8","DRY-L9")

DRY_2022 <- subset(DRY_wide, YEAR=="2022") %>% subset(., PLOT%in%plots_dryl)%>% #subset(., SUBSITE=="DRY-L")%>%
  discard(~all(is.na(.) | . ==""))
#DRY <- DRY %>% replace(is.na(.), 0)

str(DRY_2022)
vegdatapca.df <- as.data.frame(DRY_2022)
str(vegdatapca.df)
rownames(vegdatapca.df) <- vegdatapca.df$PLOT
head(vegdatapca.df)
summary(vegdatapca.df)

# It should have columns for species and a column for the year.

colSums(is.na(vegdatapca.df))
vegdatapca.df <- vegdatapca.df %>% replace(is.na(.), 0)
# If your data is in wide format (one column per species), you may need to convert it to long format.
# Example assuming your species columns are named species1, species2, etc.

# Now, 'long_data' should have columns: 'year', 'species', and 'abundance'.

# Perform PCA
pca_result <- prcomp(vegdatapca.df[,-c(1:3)], scale. = TRUE)  # Exclude 'year' from PCA

# Summary of PCA
summary(pca_result)

# Scree plot to visualize variance explained by each PC
screeplot(pca_result, type = "line")

# Biplot to visualize the relationship between samples and species
biplot(pca_result)

# Plot PC scores over time
scores <- as.data.frame(pca_result$x)  # Extract PC scores
scores$year <- as.factor(vegdatapca.df$YEAR)           # Add the 'year' column
scores$PLOT <- vegdatapca.df$PLOT 
scores <- merge(scores, treatment, by="PLOT", all.x=T)

# Extract loadings (contributions of each species to each PC)
loadings <- as.data.frame(pca_result$rotation)

# Plot using ggplot2
ggplot(scores, aes(x = PC1, y = PC2, color = factor(TREATMENT))) +
  geom_point() +
  labs(title = "PCA of Vegetation Data Over Time",
       x = "PC1",
       y = "PC2",
       color = "Treatment")


loadings$species <- rownames(loadings)
loadings %>% select(species, PC1, PC2)

# Calculate the explained variation for each PC
explained_var <- pca_result$sdev^2 / sum(pca_result$sdev^2) * 100

ggplot(scores, aes(x = PC1, y = PC2)) +
  geom_segment(data = loadings %>% select(species, PC1, PC2),
               aes(x = 0, y = 0, xend = PC1, yend = PC2, color = species),
               arrow = arrow(length = unit(0.2, "cm")),
               size = 0.5) +  # Add arrows for species contributions
  scale_color_viridis(discrete = TRUE, option = "viridis", name = "Species") +  # Use viridis color scale for species
  theme_bw()+
  geom_encircle(aes(x = PC1, y = PC2, group = factor(TREATMENT),linetype = factor(TREATMENT))) +  #, alpha = 0.2 Add circles around points with the same year
  geom_point(aes(shape=TREATMENT)) +
  geom_text(aes(label = PLOT), size = 3, vjust = -0.5, color = "black") +  # Add plot_id labels
  labs(title = "PCA of Vegetation in Dryas subsite 2022",
       x = paste("PC1: ", round(explained_var[1], 2), "%"),
       y = paste("PC2: ", round(explained_var[2], 2), "%"),
       color = "Year")
  
getwd()
ggsave("./plots/multivariate/pca_2022.png", height=10, width= 15)
