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
#browser()  # browse to ensure rows are selected correctly
names(allendalen)
# Calculate row sums for columns with a dot in their name
allendalen$plot_biomass <- rowSums(
  allendalen[, grep("\\.", names(allendalen))], 
  na.rm = TRUE
)

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

#create a longer format. exclude all the non-species columns#
#Dryaslong <- pivot_longer(Dryas_PerPlot, cols=names(Dryas_PerPlot)[5:26], names_to = "species",values_to = "abundance") #make sure that recorder is not included
#allendalen.long <- pivot_longer(allendalen, cols=names(allendalen)[c(4:25,27:31)], names_to = "species",values_to = "abundance") #make sure that the columns are correc

Dryaslong <- pivot_longer(
  Dryas_PerPlot, 
  cols = grep("\\.", names(Dryas_PerPlot), value = TRUE), # select columns with a dot in the name
  names_to = "species",
  values_to = "abundance"
)

# Similarly for `allendalen`
allendalen.long <- pivot_longer(
  allendalen, 
  cols = grep("\\.", names(allendalen), value = TRUE), # select columns with a dot in the name
  names_to = "species", 
  values_to = "abundance"
)

write.csv(unique(allendalen.long$species), "./data/secondaryData/Species_Endalen.csv")


write.csv(unique(allendalen.long$species), here("data", "secondaryData", "Species_Endalen.csv"))


datalonger <- subset(allendalen.long, TREATMENT %in% c("CTL", "OTC") & SUBSITE %in% c("CAS-L", "DRY-L"))


# Final plots, again using here() to save output images
ggplot(datalonger, aes(x=species, y=abundance, color=as.factor(YEAR))) +
  geom_boxplot() + 
  scale_color_viridis_d() +
  labs(color = "Year") +
  coord_flip() +
  theme_bw()
ggsave(here(plotdir, paste("boxplot", names(datasets)[i], ".png")), height=14, width=10)

# Additional loop and filtering steps with ggsave for final results
ggsave(here(plotdir, paste("species_years", names(datasets)[i], ".png")), height=14, width=10)



s1 <- subset(dryCAS, TREATMENT=="CTL")
summary(lm(plot_biomass~SUBSITE ,subset(s1, YEAR=="2022")))



library(lme4)

# Fit the mixed-effects model
m_mixed_i <- lmer(plot_biomass ~ TREATMENT+ SUBSITE + (1 | YEAR), data = dryCAS)
summary(m_mixed_i)

m_mixed_3way <- lmer(plot_biomass ~ YEAR*SUBSITE*TREATMENT + (SUBSITE|YEAR),data = dryCAS)#annes suggestion of 3 way interaction
summary(m_mixed_3way)
m_mixed_2way <- lmer(plot_biomass ~ YEAR*SUBSITE +YEAR*TREATMENT + (SUBSITE|YEAR),data = dryCAS)#annes suggestion of 3 way interaction
summary(m_mixed_2way)
m_mixed_t0 <- lmer(plot_biomass ~ YEAR + SUBSITE+ (SUBSITE|YEAR),data = dryCAS)
AIC(m_mixed_3way, m_mixed_2way,m_mixed_t0)

anova(m_mixed_3way,m_mixed, m_mixed_t,m_mixed_t0,m_mixed_null, m_mixed_i )
anova(m_mixed_t,m_mixed_t0)


m_bmt_LH<- lm(plot_biomass ~ TREATMENT+ SUBSITE + YEAR, data=dryCAS_LH)
m_bmt<- lm(plot_biomass ~ TREATMENT+ SUBSITE + YEAR, data=dryCAS)
summary(m_bmt)
summary(m_bmt_LH)
m_bm<- lm(plot_biomass ~ TREATMENT+ YEAR, data=dryCAS)
summary(m_bm)
m_bmy<- lm(plot_biomass ~ YEAR, data=dryCAS)
summary(m_bmy)

m_interaction <- lm(plot_biomass ~ YEAR* TREATMENT  + SUBSITE*YEAR, data = dryCAS)
summary(m_interaction)

m_3interaction <- lm(plot_biomass ~ TREATMENT * YEAR * SUBSITE, data = dryCAS)
summary(m_3interaction)

AIC(m_bmy,m_bmt,m_bm,m_interaction,m_3interaction) #3way interaction is worse

drycas2022 <- subset(dryCAS_LH, YEAR=="2022")
m_bmt_LH_2022<- aov(plot_biomass ~ SUBSITE, data=dryCAS_LH)
summary(m_bmt_LH_2022)

tukey_subsite <- emmeans(m_bmt_LH_2022, specs = ~SUBSITE, adjust = "tukey")
tukey_subsite


# Model for the OTC treatment group
m_otc <- lm(plot_biomass ~ YEAR, data = subset(dryCAS, TREATMENT == "OTC"))
summary(m_otc)

# Model for the CTL (control) treatment group
m_ctl <- lm(plot_biomass ~ YEAR, data = subset(dryCAS, TREATMENT == "CTL"))
summary(m_ctl)


# Model for the OTC treatment group
m_treat <- lm(plot_biomass ~ TREATMENT, data = dryCAS)
summary(m_treat)


Dryaslong <- subset(allendalen.long, SUBSITE == "DRY-L")
write.csv(unique(allendalen.long$species), here("data", "secondaryData", "Species_Endalen.csv"))



datasets <- list(Dryaslong,allendalen.long)
names(datasets) <- c("DRYAS","AllData")
#Dryas_PerPlot <-data2022_wide


library(lme4)
#for(i in 1){ #1:2
# browser()
i = 2

datalonger <- subset(allendalen.long, TREATMENT %in% c("CTL", "OTC") & SUBSITE %in% c("CAS-L", "DRY-L"))
write.csv(datalonger, here("data", "secondaryData","alldata_long_abundance.csv"), row.names=F)


lmTREAT<- lm(abundance~TREATMENT + SUBSITE + YEAR ,datalonger)
summary(lmTREAT)

lmeTREAT <- lmer(abundance ~ TREATMENT+ SUBSITE + (1 | YEAR), data = datalonger)
summary(lmeTREAT)

lmTREAT<- lm(abundance~TREATMENT + SUBSITE + species ,subset(datalonger, YEAR=="2022"))
summary(lmTREAT)

lmTREAT<- lm(abundance~TREATMENT * species ,subset(datalonger, YEAR=="2022"))
summary(lmTREAT)


ggplot(datalonger, aes(x=species, y=abundance, color = as.factor(YEAR)))+
  geom_boxplot() + 
  scale_color_viridis_d()+
  labs(color = "Year")+
  coord_flip()+
  theme_bw()
ggsave(paste("./plots/veg/boxplot", names(datasets)[i], ".png"))
#browser()
ggplot(datalonger, aes(x=species, y=abundance, color = TREATMENT))+
  geom_boxplot() + 
  scale_color_viridis_d()+
  facet_grid(rows = vars(as.factor(YEAR)))+
  labs(color = "Treatment")+
  coord_flip()+
  theme_bw()
ggplot(datalonger, aes(x=species, y=abundance, color = as.factor(YEAR)))+
  geom_boxplot() + 
  scale_color_viridis_d()+
  facet_grid(rows = vars(TREATMENT))+
  labs(color = "Year")+
  coord_flip()+
  theme_bw()
ggsave(paste("./plots/veg/boxplot",names(datasets)[i], "_treatment.png"), height=14, width = 10)
#if(i==1){
#  ggplot(datalonger, aes(x=species, y=abundance, color = as.factor(Recorder)))+
#    geom_boxplot() + 
#    scale_color_viridis_d()+
#    #facet_grid(rows = vars(TREATMENT))+
#    labs(color = "Recorder")+
#    coord_flip()+
#    theme_bw()
#  
#  datalonger %>%
#    group_by(species, Recorder) %>%
#    summarize(mean_abundance = mean(abundance, na.rm=T),
#              se = sd(abundance) / sqrt(n())) %>%
#    ggplot(aes(x = species, y = mean_abundance, fill = as.factor(Recorder))) +
#    geom_col(position = position_dodge(width = 0.8)) +
#    geom_errorbar(
#      aes(ymin = mean_abundance - se, ymax = mean_abundance + se),
#      position = position_dodge(width = 0.8),
#      #  width = 0.3
#    ) +
#    scale_fill_viridis_d() +
#    coord_flip() +
#    theme_bw()
#} 
datalonger %>%
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
ggsave(paste("./plots/veg/species_years", names(datasets)[i], ".png"), height=14, width = 10)


if(i == 2){
  #browser()
  filtered_data <- datalonger[!(datalonger$abundance == 0 | is.na(datalonger$abundance)), ] #%>%
  #subset(., TREATMENT == c("CTL", "OTC"))
  filtered_data$loc <- substr(filtered_data$SUBSITE, 5,5 )#nchar(filtered_data$SUBSITE)
  for(j in unique(filtered_data$loc)){
    ggplot(subset(filtered_data, loc==j), aes(x=species, y=abundance, color = as.factor(YEAR)))+
      geom_boxplot() + 
      scale_color_viridis_d()+
      facet_grid(SUBSITE~TREATMENT, drop=TRUE,  scales="free",space = "free")+
      coord_flip()+
      labs(color = "Year")+
      theme_bw()+
      theme(text = element_text(size = 18))+  # Adjust the text size as needed
      scale_y_log10()
    ggsave(paste("./plots/veg/boxplot_L",names(datasets)[i],j, "_SUBSITE.png"), height=12, width = 12)
  }
  filtered_data %>%
    group_by(species,SUBSITE, YEAR) %>%
    summarize(mean_abundance = mean(abundance, na.rm=T),
              se = sd(abundance) / sqrt(n())) %>%
    ggplot(aes(x = species, y = mean_abundance, fill = as.factor(YEAR))) +
    geom_col(position = position_dodge(width = 0.8)) +
    geom_errorbar(
      aes(ymin = mean_abundance - se, ymax = mean_abundance + se),alpha=0.5,
      position = position_dodge(width = 0.8),
      #  width = 0.3
    ) +
    scale_fill_viridis_d() +
    facet_grid(rows = vars(SUBSITE), drop=TRUE,  scales="free",space = "free")+
    coord_flip() +
    labs(fill = "Year")+
    scale_y_log10()+
    theme_bw()
  ggsave(paste("./plots/veg/species_years_SUBSITES", names(datasets)[i], ".png"), height=25, width = 8)
  
  
}

#}




