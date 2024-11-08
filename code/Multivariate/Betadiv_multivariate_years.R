####NMDS for vascular plants####

data_multi_all <-read.csv("./data/secondaryData/subsetDryasCassiope.csv")
# Replace NA values with 0
data_multi_all[is.na(data_multi_all)] <- 0

####create NMDS to see effect of treatment and time for subsites####
# Prepare data for NMDS
summary <- data_multi_all[1:5]  # Summary data
wide_list <- data_multi_all[6:(ncol(data_multi_all)-1)]  # Species data

head(wide_list)
head(summary)

# NMDS
as.bray.NMDS <- metaMDS(wide_list, distance = "bray")

ordiplot(as.bray.NMDS)
stressplot(as.bray.NMDS)

species_scores <- scores(as.bray.NMDS, display = "species")
species_scores_complete <- species_scores[complete.cases(species_scores), ]
# Plot NMDS
png("./plots/ordi_all_treat.png", width = 1500, height = 1150, units = "px", res = 250)
ordiplot(as.bray.NMDS)
points(as.bray.NMDS, col = ifelse(data_multi_all$TREATMENT == "CTL", "darkgreen","yellow"))
ordiellipse(as.bray.NMDS, groups = summary$YEAR, draw = "polygon", col = "grey90", label = TRUE)
ordiellipse(as.bray.NMDS, groups = summary$SUBSITE, draw = "polygon", col = "red", label = TRUE)
#ordiellipse(as.bray.NMDS, groups = summary$TREATMENT, draw = "polygon", col = "orange", label = TRUE)
#points(as.bray.NMDS, col = ifelse(data_multi_all$TREATMENT == "CTL", "yellow", "blue"))
#text(species_scores_complete, labels = rownames(species_scores_complete))
dev.off()

# Check factors influencing the plot
#bray.fit <- envfit(as.bray.NMDS ~ TREATMENT, data = summary, perm = 999)
bray.fit_cont <- envfit(as.bray.NMDS ~ YEAR, data = summary, perm = 999)

# Plot environmental factors
png(paste("./plots/envfit_", "all_treatment", ".png", sep = ""), width = 1500, height = 1150, units = "px", res = 250)
ordiplot(as.bray.NMDS)
#plot(bray.fit)
plot(bray.fit_cont)
dev.off()

unique_subsites <- unique(data_multi_all$SUBSITE)

# Loop through each subsite
for (subsite in unique_subsites) {
  # Subset data for the current subsite
  data_subsite <- subset(data_multi_all, SUBSITE == subsite)
  
  # Prepare data for NMDS
  summary <- data_subsite[1:5]  # Summary data
  wide_list <- data_subsite[6:ncol(data_subsite)]  # Species data
  
  head(wide_list)
  head(summary)
  
  # NMDS
  as.bray.NMDS <- metaMDS(wide_list, distance = "bray")
  
  ordiplot(as.bray.NMDS)
  stressplot(as.bray.NMDS)
  
  summary$SUBSITE_YEAR <- interaction(summary$YEAR, summary$TREATMENT)
  
  # Plot NMDS
  png(paste("./plots/ordi_subsite_TREAT_", subsite, ".png", sep = ""), width = 1500, height = 1150, units = "px", res = 250)
  ordiplot(as.bray.NMDS)
  ordiellipse(as.bray.NMDS, groups = summary$YEAR, draw = "polygon", col = "grey90", label = TRUE)
  ordiellipse(as.bray.NMDS, groups = summary$TREATMENT, draw = "polygon", col = "orange", label = TRUE)
  points(as.bray.NMDS, display = "sites", col = summary$COLOR, pch = 21, bg = summary$COLOR)
  dev.off()
  
  
  summary$YEAR_TREATMENT <- interaction(summary$YEAR, summary$TREATMENT)
  years <- sort(unique(summary$YEAR))
  # Yellow gradient for control
  control_colors <- viridis(length(years), option = "D", begin = 0.8, end = 1)
  # Purple gradient for treatment
  treatment_colors <- viridis(length(years), option = "D", begin = 0, end = 0.2)
 # browser()
  # Create a color mapping for each year and treatment
  color_map <- data.frame(
    YEAR = rep(years, 2),
    TREATMENT = rep(c("CTL", "OTC"), each = length(years)),
    COLOR = c(control_colors, treatment_colors)
  )
  # Assign colors to the combined factor based on year and treatment
  summary$COLOR <- apply(summary, 1, function(row) {
    color_map$COLOR[which(color_map$YEAR == row["YEAR"] & color_map$TREATMENT == row["TREATMENT"])]
  })
  
  png(paste("./plots/ordi_subsite_YTREAT_", subsite, ".png", sep = ""), width = 1500, height = 1150, units = "px", res = 250)
  ordiplot(as.bray.NMDS)
  unique_groups <- unique(summary$YEAR_TREATMENT)
  unique_groups <- unique(summary$YEAR_TREATMENT)
  for (group in unique_groups) {
    group_indices <- which(summary$YEAR_TREATMENT == group)
    group_color <- unique(summary$COLOR[group_indices])
    ordiellipse(as.bray.NMDS, groups = summary$YEAR_TREATMENT, draw = "polygon", col = group_color, label = TRUE)
  }
  ordiellipse(as.bray.NMDS, groups = summary$YEAR_TREATMENT, draw = "polygon", col = summary$COLOR, label = TRUE)
  #points(as.bray.NMDS, col = ifelse(data_multi_all$TREATMENT == "CTL", "yellow", "blue"))
  #text(species_scores_complete, labels = rownames(species_scores_complete))
  dev.off()
  
  species_scores <- scores(as.bray.NMDS, display = "species")
  species_scores_complete <- species_scores[complete.cases(species_scores), ]
  png(paste("./plots/ordi_subsite_SYTREAT_", subsite, ".png", sep = ""), width = 1500, height = 1150, units = "px", res = 250)
  ordiplot(as.bray.NMDS)
  unique_groups <- unique(summary$YEAR_TREATMENT)
  unique_groups <- unique(summary$YEAR_TREATMENT)
  for (group in unique_groups) {
    group_indices <- which(summary$YEAR_TREATMENT == group)
    group_color <- unique(summary$COLOR[group_indices])
    ordiellipse(as.bray.NMDS, groups = summary$YEAR_TREATMENT, draw = "polygon", col = group_color, label = TRUE)
  }
  ordiellipse(as.bray.NMDS, groups = summary$YEAR_TREATMENT, draw = "polygon", col = summary$COLOR, label = TRUE)
  points(as.bray.NMDS, col = ifelse(data_multi_all$TREATMENT == "CTL", "yellow", "blue"))
  text(species_scores_complete, labels = rownames(species_scores_complete))
  dev.off()
  
  # Check factors influencing the plot
  bray.fit <- envfit(as.bray.NMDS ~ SUBSITE, data = summary, perm = 999)
  bray.fit_cont <- envfit(as.bray.NMDS ~ YEAR, data = summary, perm = 999)
  fit <- envfit(as.bray.NMDS, summary[, c("YEAR", "TREATMENT")], permutations = 999)
  
  # Plot environmental factors
  png(paste("./plots/envfit_TREAT_", subsite, ".png", sep = ""), width = 1500, height = 1150, units = "px", res = 250)
  ordiplot(as.bray.NMDS)
  plot(fit)
  #plot(bray.fit_cont)
  dev.off()
}

####create NMDS for all controll plots (both subsites and subsites seperately)####
data_multi <- subset(data_multi_all, TREATMENT=="CTL")
data_multi_OTC <- merge(data_multi, subset(data_multi_all, (TREATMENT=="OTC"&YEAR=="2022")), all=T)



data_subsite <- data_multi

# Prepare data for NMDS
summary <- data_subsite[1:5]  # Summary data
wide_list <- data_subsite[6:ncol(data_subsite)]  # Species data

head(wide_list)
head(summary)

# NMDS
as.bray.NMDS <- metaMDS(wide_list, distance = "bray")

ordiplot(as.bray.NMDS)
stressplot(as.bray.NMDS)

# Plot NMDS
png("./plots/ordi_all.png", width = 1500, height = 1150, units = "px", res = 250)
ordiplot(as.bray.NMDS)
ordiellipse(as.bray.NMDS, groups = summary$YEAR, draw = "polygon", col = "grey90", label = TRUE)
ordiellipse(as.bray.NMDS, groups = summary$SUBSITE, draw = "polygon", col ="red", label = TRUE)
dev.off()

# Check factors influencing the plot
#bray.fit <- envfit(as.bray.NMDS ~ TREATMENT, data = summary, perm = 999)
bray.fit_cont <- envfit(as.bray.NMDS ~ YEAR, data = summary, perm = 999)

# Plot environmental factors
png(paste("./plots/envfit_", "all", ".png", sep = ""), width = 1500, height = 1150, units = "px", res = 250)
ordiplot(as.bray.NMDS)
#plot(bray.fit)
plot(bray.fit_cont)
dev.off()


dummy<-specnumber(data_multi[,-c(1:5)]) #remove non_plants from the dataset!!!!!!!

# Get unique subsites
unique_subsites <- unique(data_multi$SUBSITE)

# Loop through each subsite
for (subsite in unique_subsites) {
  # Subset data for the current subsite
 # data_subsite <- subset(data_multi, SUBSITE == subsite)
  data_subsite <- subset(data_multi_OTC, SUBSITE == subsite)
  browser()
  data_multi_OTC
  # Prepare data for NMDS
  summary <- data_subsite[1:5]  # Summary data
  wide_list <- data_subsite[6:ncol(data_subsite)]  # Species data
  
  head(wide_list)
  head(summary)
  
  # NMDS
  as.bray.NMDS <- metaMDS(wide_list, distance = "bray")
  
  ordiplot(as.bray.NMDS)
  stressplot(as.bray.NMDS)
  
  # Plot NMDS
  png(paste("./plots/ordi_subsite_", subsite, ".png", sep = ""), width = 1500, height = 1150, units = "px", res = 250)
  ordiplot(as.bray.NMDS)
  ordiellipse(as.bray.NMDS, groups = summary$YEAR, draw = "polygon", col = "grey90", label = TRUE)
  dev.off()
  
  species_scores <- scores(as.bray.NMDS, display = "species")
  species_scores_complete <- species_scores[complete.cases(species_scores), ]
  png(paste("./plots/ordi_Sp_subsite_", subsite, ".png", sep = ""), width = 1500, height = 1150, units = "px", res = 250)
  ordiplot(as.bray.NMDS)
  ordiellipse(as.bray.NMDS, groups = summary$YEAR, draw = "polygon", col = "grey90", label = TRUE)
  ordiellipse(as.bray.NMDS, groups = summary$TREATMENT, draw = "polygon", col ="orange", label = TRUE)
  text(species_scores_complete, labels = rownames(species_scores_complete))
  dev.off()
  
  # Check factors influencing the plot
  bray.fit <- envfit(as.bray.NMDS ~ SUBSITE, data = summary, perm = 999)
  bray.fit_cont <- envfit(as.bray.NMDS ~ YEAR, data = summary, perm = 999)
  
  # Plot environmental factors
  png(paste("./plots/envfit_", subsite, ".png", sep = ""), width = 1500, height = 1150, units = "px", res = 250)
  ordiplot(as.bray.NMDS)
  plot(bray.fit)
  plot(bray.fit_cont)
  dev.off()
}
####check 2022 for treatment####
data_2022 <- subset(data_multi_all, YEAR=="2022")
# Prepare data for NMDS
summary <- data_2022[1:5]  # Summary data
wide_list <- data_2022[6:ncol(data_2022)]  # Species data

head(wide_list)
head(summary)

# NMDS
as.bray.NMDS <- metaMDS(wide_list, distance = "bray")

ordiplot(as.bray.NMDS)
stressplot(as.bray.NMDS)

summary$SUBSITE_TREATMENT <- interaction(summary$SUBSITE, summary$TREATMENT)

# Plot NMDS
png("./plots/ordi_all_treat_2022.png", width = 1500, height = 1150, units = "px", res = 250)
ordiplot(as.bray.NMDS)
ordiellipse(as.bray.NMDS, groups = summary$SUBSITE, draw = "polygon", col = "red", label = TRUE)
ordiellipse(as.bray.NMDS, groups = summary$TREATMENT, draw = "polygon", col = "orange", label = TRUE)
dev.off()

# Check factors influencing the plot
#bray.fit <- envfit(as.bray.NMDS ~ TREATMENT, data = summary, perm = 999)
bray.fit_cont <- envfit(as.bray.NMDS ~ YEAR, data = summary, perm = 999)

# Plot environmental factors
png(paste("./plots/envfit_2022_", "all_treatment", ".png", sep = ""), width = 1500, height = 1150, units = "px", res = 250)
ordiplot(as.bray.NMDS)
#plot(bray.fit)
plot(bray.fit_cont)
dev.off()

unique_subsites <- unique(data_2022$SUBSITE)

# Loop through each subsite
for (subsite in unique_subsites) {
  # Subset data for the current subsite
  data_subsite <- subset(data_2022, SUBSITE == subsite)
  
  # Prepare data for NMDS
  summary <- data_subsite[1:5]  # Summary data
  wide_list <- data_subsite[6:ncol(data_subsite)]  # Species data
  
  head(wide_list)
  head(summary)
  
  # NMDS
  as.bray.NMDS <- metaMDS(wide_list, distance = "bray")
  
  ordiplot(as.bray.NMDS)
  stressplot(as.bray.NMDS)
  
  # Plot NMDS
  png(paste("./plots/ordi_subsite_TREAT_2022", subsite, ".png", sep = ""), width = 1500, height = 1150, units = "px", res = 250)
  ordiplot(as.bray.NMDS)
  ordiellipse(as.bray.NMDS, groups = summary$TREATMENT, draw = "polygon", col = "orange", label = TRUE)
  dev.off()
  
  # Check factors influencing the plot
  bray.fit <- envfit(as.bray.NMDS ~ SUBSITE, data = summary, perm = 999)
  bray.fit_cont <- envfit(as.bray.NMDS ~ YEAR, data = summary, perm = 999)
  
  # Plot environmental factors
  png(paste("./plots/envfit_TREAT_2022", subsite, ".png", sep = ""), width = 1500, height = 1150, units = "px", res = 250)
  ordiplot(as.bray.NMDS)
  plot(bray.fit)
  plot(bray.fit_cont)
  dev.off()
}
