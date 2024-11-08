#install.packages("tidyverse")
library("tidyverse")
library("readxl")
setwd("~/Documents/Uni/Masterarbeit/Data_Analysis_Endalen")
###load NDVI NDVI data####
NDVI_m.df<- read_xlsx("./data/Env/NDVI_Endalen.xlsx")

#names(NDVI_m.df) <-  gsub( "X","",names(NDVI_m.df)) #replace x in the column name (only run if the rest of the code dosen't work)
NDVI_m.df <- as.data.frame(NDVI_m.df) #converts tibble to dataframe

NDVI_m.df$Plot <-  gsub( "Bis","BIS",NDVI_m.df$Plot) %>% gsub( "Dry","DRY",.) %>% gsub( "Cas","CAS",.)


#create a long format with one column for the plots, one for date and one for the  values
NDVI_m.long <- pivot_longer(NDVI_m.df, cols = names(NDVI_m.df)[-1], names_to="date_p", 
                              values_to="NDVI",values_transform = as.numeric) 
#Create a column for the subsites (extract from the plot name)
NDVI_m.long$SUBSITE <- substr(NDVI_m.long$Plot, 1, 5) 
#create a column for the position if it was measured in the upper right(TR) upper left (TL) or bottom right (BR) corner
NDVI_m.long$Position <- substr(NDVI_m.long$date_p, 10, 10) 
#create one column for the date
NDVI_m.long$date <- dmy(substr(NDVI_m.long$date_p, 1, 8))


#### Plot the  data####
#create a first plot (each measurement in each corner is treated as replica, therefore: DON'T use this plot)
ggplot(NDVI_m.long, aes(x=date, y=NDVI, color=as.character(SUBSITE)))+
  geom_point()+
  ylab("NDVI")+
  xlab("Date")+
  scale_colour_viridis_d(
    option = "D",
    aesthetics = "colour",
    name="Habitat"
  )+
  theme_bw()

# create a dataframe with the means over the measurements in each corner
NDVI_m.long2 <- pivot_wider(NDVI_m.long[,-2], names_from=Position, values_from = NDVI)
NDVI_m.long2$mean_NDVI <- rowMeans(NDVI_m.long2[,-(1:3)], na.rm=T)
treatment <- read.csv("./data/plot_treatments.csv")
NDVI.long2 <- merge(NDVI_m.long2, treatment, all.x=T, by.x="Plot", by.y = "PLOT")

getwd()
setwd("/Users/ms/Documents/Uni/Masterarbeit/Data_Analysis_Endalen/data/secondaryData/ENV")
write.csv(NDVI.long2, "./NDVI_clean.csv",row.names = FALSE) # saves the clean dataframe so I can use it for other analysis
setwd("/Users/ms/Documents/Uni/Masterarbeit/Data_Analysis_Endalen/data/secondaryData/ENV")

NDVI_CAS_dry <- subset(NDVI.long2, TREATMENT %in% c("CTL", "OTC")) %>%
  subset(. , SUBSITE %in% c("CAS-L", "DRY-L", "CAS-H", "DRY-H"))


path_plots <- "/Users/ms/Documents/Uni/Masterarbeit/Data_Analysis_Endalen/plots/env/NDVI"
setwd(path_plots)
ggplot(NDVI.long2, aes(x=date, y=mean_NDVI, color=as.character(TREATMENT)))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw()+
  facet_grid(rows="SUBSITE")

subset_NDVI <- subset(NDVI.long2, date=="2023-07-26")%>%
  subset(. , TREATMENT %in% c("CTL", "OTC")) %>%
  subset(. , SUBSITE %in% c("CAS-L", "DRY-L", "CAS-H", "DRY-H"))
                      
ggplot(subset_NDVI, aes(x=SUBSITE, y=mean_NDVI, fill=as.factor(TREATMENT)))+
  geom_boxplot()+
  ylab("NDVI")+
  xlab("Habitat")+
  scale_fill_viridis_d(
    alpha = 1,
    begin = 0.46,
    end = 0.8,
    option = "D",
    aesthetics = "fill",
    name="Treatment"
  )+
  theme_bw()
ggsave("./NDVI_mean_subsites.png",  height=3.5, width=5)


#test different ANOVAS for NDVI
m_st <- aov(mean_NDVI~SUBSITE + TREATMENT,subset_NDVI )
anova(m_st)
m_s_t <- aov(mean_NDVI~SUBSITE * TREATMENT,subset_NDVI )
anova(m_s_t)
m_t <- aov(mean_NDVI~TREATMENT,subset_NDVI )
anova(m_t)
m_s <- aov(mean_NDVI~ SUBSITE ,subset_NDVI )
m_sLM <- lm(mean_NDVI~ SUBSITE ,subset_NDVI )
summary(m_s)
summary(m_sLM)
anova(m_s)
AIC(m_st,m_s_t, m_s,m_t)

install.packages("AICcmodavg")
library(AICcmodavg)
aictab(cand.set = c(m_st,m_s_t, m_s,m_t), modnames = model.names)


posthoc_result <- TukeyHSD(m_s, group=T)
posthoc_result


ggplot(NDVI_CAS_dry, aes(x=date, y=mean_NDVI, color=as.character(SUBSITE)))+
  geom_jitter(alpha=0.8,width = 0.3 )+
  geom_smooth(method="lm")+
  ylab("NDVI")+
  xlab("Date")+
  scale_colour_viridis_d(
    option = "D",
    aesthetics = "colour",
    name="Habitat"
  )+
  theme_bw()
ggsave("./NDVI_time_subsites.png",  height=3.5, width=5)


ggplot(NDVI_CAS_dry, aes(x=as.factor(date), y=mean_NDVI, color=SUBSITE))+
  geom_boxplot(alpha=0.8)+
  geom_smooth(method="lm")+
  ylab("NDVI")+
  xlab("Date")+
  scale_colour_viridis_d(
    option = "D",
    aesthetics = "colour",
    name="Habitat"
  )+
  theme_bw()
ggsave("./date_NDVIM_subsites.png", height=3.5, width=5)


model_ndvi_subsite<- lm(data=NDVI.long2, mean_NDVI~ SUBSITE)
model_ndvi_subsite<- lm(data=NDVI.long2, mean_NDVI~ TREATMENT+ SUBSITE)
summary(model_ndvi_subsite)


#####Check if the increase in NDVI is different among the subsites####

names(NDVI.long2)

# Load the necessary library

# Fit repeated measures ANOVA
anova_result <- aov(mean_NDVI ~ SUBSITE * date + Error(Plot/date), data = NDVI.long2)

# Summary of ANOVA
summary(anova_result)

# Load the necessary library
library(lme4)

# Fit linear mixed-effects model
lme_model <- lmer(mean_NDVI ~ SUBSITE * date + (1|Plot), data = NDVI.long2)

# Summary of the model
summary(lme_model)

# Assuming your data frame is NDVI.long2

# Calculate the increase in NDVI for each subsite
NDVI.long2$Increase <- ave(NDVI.long2$mean_NDVI, NDVI.long2$SUBSITE, FUN = function(x) c(0, diff(x)))

# Load the necessary libraries
library(lme4)
library(emmeans)

# Fit linear mixed-effects model
lme_model <- lmer(Increase ~ SUBSITE * date + (1|Plot), data = NDVI.long2)

# Summary of the model
summary(lme_model)

# Post hoc tests for pairwise comparisons
emmeans_result <- emmeans(lme_model, ~ SUBSITE | date)

# Summary of pairwise comparisons
summary(emmeans_result)



# Calculate the increase in NDVI for each subsite
NDVI.long2$Increase <- ave(NDVI.long2$mean_NDVI, NDVI.long2$SUBSITE, FUN = function(x) c(0, diff(x)))

# Load the necessary library
library(car)

# Fit ANOVA
anova_result <- aov(Increase ~ SUBSITE, data = NDVI.long2)

# Summary of ANOVA
summary(anova_result)


# Assuming your data frame is ndvi_data

# Calculate the increase in NDVI for each subsite
NDVI.long2$Increase <- ave(NDVI.long2$mean_NDVI, NDVI.long2$SUBSITE, FUN = function(x) c(0, diff(x)))

# Load the necessary library
library(car)


# Assuming your data frame is ndvi_data

# Fit ANOVA
anova_result <- aov(Increase ~ SUBSITE, data = NDVI.long2)

# Summary of ANOVA
summary(anova_result)

# Post-hoc tests for pairwise comparisons
posthoc_result <- TukeyHSD(anova_result)

# Display post-hoc results
print(posthoc_result)



# Fit linear regression for BIS-L subsite
lm_BIS_L <- lm(Increase ~ date, data = subset(NDVI.long2, SUBSITE == "BIS-L"))

# Summary of the regression
summary(lm_BIS_L)

# Fit linear regression for BIS-H subsite
lm_BIS_H <- lm(Increase ~ date, data = subset(NDVI.long2, SUBSITE == "BIS-H"))

# Summary of the regression
summary(lm_BIS_H)
