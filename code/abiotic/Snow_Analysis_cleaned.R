####  Analysis of the snow                                                  ####
#     Author: Merle A. Scheiner  , 2024                                       #
###############################################################################

# Load necessary packages
library(stats)
library(ggplot2)
library(emmeans)
library(tidyverse)

setwd("~/Documents/Uni/Masterarbeit/Data_Analysis_Endalen")
treatment <- read.csv("./data/plot_treatments.csv")

### SNOW TRANSECT SNOWBED####
transect<- read.csv("./data/env/Snowbed_TRANSECT.csv")
#transect$SUBSITE <-
rep("BIS-H", nrow(transect)/2)
xx <- factor(sort(rank(row.names(transect))%%2))
levels(xx) = c("BIS-H", "BIS-L")
transect$SUBSITE <- xx


## estimate snow depth for the SNOWbed####
half_tansects <- split(transect, factor(sort(rank(row.names(transect))%%2)))
names(half_tansects) <-  c("BIS-H", "BIS-L")
BIS_H_snowmean <- mean(half_tansects[[1]]$depth_cm)
BIS_L_snowmean <- mean(half_tansects[[2]]$depth_cm)

#### all the other sites ####
mast<- read.csv("./data/env/SNOW_mast.csv")
plots<- read.csv("./data/env/SNOW_plots.csv")
plots[plots$SUBSITE=="BIS-H",]$SNOW_DEPTH_CM <- BIS_H_snowmean
plots[plots$SUBSITE=="BIS-L",]$SNOW_DEPTH_CM <- BIS_L_snowmean

allsnow <- merge(plots, mast, all=T)
allsnow <- merge(allsnow[, !(names(allsnow) %in% c("TREATMENT"))], treatment, by="PLOT", all.x=T) #remove treatment from original plot

write.csv(allsnow, "./data/secondaryData/ENV/all_snowdata.csv", row.names = FALSE)

####Snowdepthplot####
#snowDC <- allsnow #for the analsis of the whole data

snow_DC <- subset(allsnow, !(SUBSITE %in% c("BIS-H", "BIS-L")))%>%subset(. , TREATMENT %in% c("CTL", "OTC"))

snowdepth.plot <- ggplot(snow_DC, aes(x=SUBSITE, y=SNOW_DEPTH_CM, color=TREATMENT))+
  geom_boxplot()+
  geom_point(aes(x=SUBSITE,y=SNOW_MAST), color="black")+
  scale_colour_viridis_d(
    option = "G",
    aesthetics = "colour",
    begin = 0,
    end = 0.8,
    name="Treatment"
  )+
  theme(text = element_text(size = 18))+
  #geom_point(aes(x=bish[2],y=as.numeric(bish[1])), color="black")+ #mean of the snowtransec
  #geom_boxplot(data=transect, aes(x=SUBSITE, y=depth_cm), color="grey")+
  coord_flip()+
  ylab("Snow depth in cm")+
  xlab("Habitat")+
  theme_bw()#,  # Increase font size
#    axis.text.y = element_text(size = 10),  # Adjust y-axis text size
#   axis.title.y = element_text(margin = margin(r = 20)))  # Adjust y-axis title margin)
snowdepth.plot
ggsave("./plots/env/snow_SUBSITE_old.png", height=3.5, width=5)

snowdepth.plot <- ggplot(snow_DC, aes(x = SUBSITE, y = SNOW_DEPTH_CM, color = TREATMENT)) +
  geom_boxplot() +
  theme_bw()+
  geom_point(aes(x = SUBSITE, y = SNOW_MAST), color = "black") +
  scale_colour_viridis_d(
    option = "G",
    aesthetics = "colour",
    begin = 0,
    end = 0.8,
    name = "Treatment"
  ) +
  theme(
    text = element_text(size = 16),  # Increase font size for all text
    #   axis.text.y = element_text(size = 16)  # Adjust y-axis text size
  ) +
  coord_flip() +
  ylab("Snow depth in cm") +
  xlab("Habitat")# +
snowdepth.plot
ggsave("./plots/env/snow_SUBSITE.png", height=3.5, width=5)




####Statistics of snow depth of DRY and CAS####

# Step 1: ANOVA for Overall Differences
# Conduct ANOVA to assess overall differences among subsites and treatment groups
anova_result <- aov(SNOW_DEPTH_CM ~ SUBSITE + TREATMENT, data = snow_DC)
summary(anova_result)
capture.output(summary(anova_result), file = "anova results_CAS_DRY_Snowdepth.txt")
# Step 2: Pairwise Comparisons
# Perform pairwise comparisons for subsites and treatment groups
tukey_subsite <- emmeans(anova_result, specs = ~SUBSITE, adjust = "tukey")
tukey_treatment <- emmeans(anova_result, specs = ~TREATMENT, adjust = "tukey")

# Display pairwise comparison results
summary(tukey_subsite)
summary(tukey_treatment)
capture.output(summary(tukey_treatment), file = "anova tukey results casl_snowdepthOTC.txt")

# Step 3: Linear Regression for Treatment Effects
# Fit linear regression model to assess treatment effects while controlling for subsite
lm_model <- lm(SNOW_DEPTH_CM ~ SUBSITE + TREATMENT, data = snow_DC)
summary(lm_model)


# Step 5: Model Evaluation
# Assess the fit of the linear regression model
# Diagnostic plots
plot(lm_model)


# Calculate average snow depth for each subsite
average_snow_depth <- aggregate(SNOW_DEPTH_CM ~ SUBSITE + TREATMENT, data = snow_DC, 
                           FUN = function(x) c(mean = mean(x, na.rm = TRUE), 
                                               sd = sd(x, na.rm = TRUE)))
average_snow_depth$mean <- average_snow_depth$SNOW_DEPTH_CM[, "mean"]
average_snow_depth$sd <- average_snow_depth$SNOW_DEPTH_CM[, "sd"]
average_snow_depth$SE <- average_snow_depth$sd / sqrt(nrow(snow_DC) / nrow(average_snow_depth))

average_snow_depth



####Differences of snow accumulation in OTC####
otc <- subset(plots, SNOW_DEPTH_NORTH!="NA")
OTC_long <- pivot_longer(otc, 
                         cols = starts_with("SNOW_DEPTH"),
                         names_to = "POSITION",
                         values_to = "SNOW_DEPTH")


ggplot(OTC_long, aes(x=SUBSITE, y=SNOW_DEPTH, color=POSITION))+
  geom_boxplot()+
  scale_colour_viridis_d(
    option = "G",
    aesthetics = "colour"
  )+
  theme_bw()
ggsave("./plots/env/OTC_accumulation.png", height=3.5, width=5)

casl_snowdepthOTC <- lm(data=subset(OTC_long, SUBSITE=="CAS-L"), SNOW_DEPTH ~ POSITION)
summary(casl_snowdepthOTC)
dryl_snowdepthOTC <- lm(data=subset(OTC_long, SUBSITE=="DRY-L"), SNOW_DEPTH ~ POSITION)
summary(dryl_snowdepthOTC)
anova(dryl_snowdepthOTC)
capture_casl_snowdepthOTC <- summary(dryl_snowdepthOTC)
capture.output(capture_casl_snowdepthOTC, file = "anova results casl_snowdepthOTC.txt")
#signifikant weniger schnee außerhalb der OTC auf den DRYAS sites.

####Snowmeltout####
######Data wragling#####
snowmeltout <- read.csv("./data/env/SNOWmeltout.csv")
names(snowmeltout) <-  gsub( "X","",names(snowmeltout))
snowmeltout$Site <-  gsub( "Dry","DRY",snowmeltout$Site)
#snowmeltout <-  gsub( "<5","2",snowmeltout)
snowmeltout[snowmeltout == "<5"] <- "2"
#convert data to numeric
snowmeltout[, -1] <- sapply(snowmeltout[, -1], as.numeric)

snowmelt_long <- pivot_longer(snowmeltout, cols = names(snowmeltout)[-1], names_to="date", 
                              values_to="snow_cover")
library(lubridate)
snowmelt_long$date <- parse_date_time(snowmelt_long$date, "dmy")

snowmelt_long$SUBSITE <- substr(snowmelt_long$Site, 1, 5)

ggplot(data=snowmelt_long, aes(x=date, y=log(snow_cover+11), color=SUBSITE))+
  geom_point()+
  geom_smooth(method="lm")+
  scale_colour_viridis_d(
    option = "D",
    aesthetics = "colour",
    name="Series Number"
  )+
  # geom_smooth(method = "glm", formula = y ~ log(x/(1-x)))+
  # geom_sigmoid()+
  # ylim(0,100)+
  theme_bw()
#  geom_smooth(method = "nls", se = FALSE,
#               formula = y ~ a/(1+exp(-b*(x-c))))

snowmelt_long <- merge(snowmelt_long, treatment, by.x="Site", by.y="PLOT", all.x=T)
result_data <- snowmelt_long %>%
  group_by(Site, TREATMENT) %>%
  arrange(date) %>%
  mutate(FirstZeroDate = ifelse(cumsum(snow_cover == 0) == 1, as.character(date), NA)) %>%
  filter(!is.na(FirstZeroDate)) %>%
  select(Site, FirstZeroDate) %>%
  unique()%>%
  merge(., unique(snowmelt_long[,c(1,4)]))

result_data$FirstZeroDate <- format(result_data$FirstZeroDate, format = "%Y-%m-%d %H:%M:%S")


result_data <- subset(result_data, !(SUBSITE %in% c("BIS-H", "BIS-L")))%>%subset(. , TREATMENT %in% c("CTL", "OTC"))


#####Statistics of snow meltout####

# Step 1: ANOVA for Overall Differences
# Conduct ANOVA to assess overall differences among subsites and treatment groups
result_data$yearday <-  as.numeric(yday(result_data$FirstZeroDate))
str(result_data)
anova_result <- aov(yearday~ SUBSITE + TREATMENT, data = result_data)
anova_result_s <- aov(yearday~ SUBSITE, data = result_data)
anova_result_t <- aov(yearday~  TREATMENT, data = result_data)
summary(anova_result_s)
summary(anova_result)
summary(anova_result_t)

AIC(anova_result, anova_result_s)
capture.output(summary(anova_result), file = "anova results_CAS_DRY_SnowMelt.txt")
# Step 2: Pairwise Comparisons
# Perform pairwise comparisons for subsites and treatment groups
tukey_subsite <- emmeans(anova_result_s, specs = ~SUBSITE, adjust = "tukey")
tukey_treatment <- emmeans(anova_result, specs = ~TREATMENT, adjust = "tukey")

# Display pairwise comparison results
summary(tukey_subsite)
summary(tukey_treatment)
capture.output(summary(tukey_subsite), file = "anova tukey results subsite_snowmel.txt")
capture.output(summary(tukey_treatment), file = "anova tukey results treat_snowmelt.txt")

# Step 3: Linear Regression for Treatment Effects
# Fit linear regression model to assess treatment effects while controlling for subsite
lm_model <- lm(yearday ~ SUBSITE + TREATMENT, data = result_data)
summary(lm_model)


# Step 5: Model Evaluation
# Assess the fit of the linear regression model
# Diagnostic plots
plot(lm_model)

# Step 6: Interpretation and Reporting
# Interpret and report the results of the analyses
# Include findings from ANOVA, pairwise comparisons, linear regression coefficients, and interaction effects

###plot the data###


ggplot(data=result_data, aes(x=as_date(FirstZeroDate), y=Site, color=SUBSITE))+
  geom_point()+
  # geom_smooth(method = "glm", formula = y ~ log(x/(1-x)))+
  # geom_sigmoid()+
  # ylim(0,100)+
  theme_bw()
#  geom_smooth(method = "nls", se = FALSE,
#               formula = y ~ a/(1+exp(-b*(x-c))))

snowmeltout.plot<- ggplot(data=result_data, aes(x=as_date(FirstZeroDate), y=SUBSITE, color=TREATMENT))+
  geom_boxplot()+
  theme_bw()+
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1))+  # Rotate x-axis labels by 45 degrees)+
  scale_colour_viridis_d(
    option = "G",
    begin = 0,
    end = 0.8,
    aesthetics = "colour",
    name="Treatment"
  )+
  ylab("Habitat")+
  xlab("Timing of Snowmelt")
snowmeltout.plot
ggsave("./plots/env/snowmeltout_bp.png", height=3.5, width=5)


##### survival analysis of snowmelt####

# Load necessary packages
library(survival)
library(survminer)
library(ggplot2)
##### without treatment####

# Convert snow_cover to a survival object
snowmelt_long$surv_obj <- with(snowmelt_long, Surv(time = as.numeric(date), event = (snow_cover == 0)))

# Fit Kaplan-Meier survival curves
km_fit <- survfit(surv_obj ~ SUBSITE, data = snowmelt_long)

# Plot Kaplan-Meier curves
g <- ggsurvplot(km_fit, data = snowmelt_long, pval = TRUE)
g
# Perform log-rank test
log_rank <- survdiff(surv_obj ~ SUBSITE, data = snowmelt_long)
print(log_rank)


# Perform pairwise comparisons between subsites
pairwise_result <- pairwise_survdiff(formula = surv_obj ~ SUBSITE, data = snowmelt_long, p.adjust.method = "bonferroni")

# Print pairwise comparison results
print(pairwise_result)


#### with treatment just for DRY and CAS####

snowmelt_long_dryCAS <- subset(snowmelt_long, TREATMENT %in% c("CTL", "OTC") & SUBSITE %in% c("CAS-L", "DRY-L"))
# Convert snow_cover to a survival object
snowmelt_long_dryCAS$surv_obj <- with(snowmelt_long_dryCAS, Surv(time = as.numeric(date), event = (snow_cover == 0)))

# Fit Kaplan-Meier survival curves
km_fit <- survfit(surv_obj ~ SUBSITE + TREATMENT, data = snowmelt_long_dryCAS)

# Plot Kaplan-Meier curves
g <- ggsurvplot(km_fit, data = snowmelt_long_dryCAS, pval = TRUE)
g
# Perform log-rank test
log_rank <- survdiff(surv_obj ~ SUBSITE+ TREATMENT, data = snowmelt_long_dryCAS)
print(log_rank)


# Perform pairwise comparisons between subsites
pairwise_result <- pairwise_survdiff(formula = surv_obj ~ SUBSITE+TREATMENT, data = snowmelt_long_dryCAS, p.adjust.method = "bonferroni")

# Print pairwise comparison results
print(pairwise_result)

#### correlate snow depth with first snow free day####
# merge the data
snow_data_cor <- merge(result_data, snow_DC[,c(1:4, 9)], all.y = T, by.x=c( "TREATMENT", "SUBSITE","Site"), by.y=c( "TREATMENT", "SUBSITE","PLOT"))
# Fit linear regression model
lm_depth_date <- lm(yearday~SNOW_DEPTH_CM, data = snow_data_cor)
lm_depth_date_treat <- lm(yearday~SNOW_DEPTH_CM + TREATMENT, data = snow_data_cor)
lm_depth_date_treat_subs <- lm(yearday~SNOW_DEPTH_CM + TREATMENT + SUBSITE, data = snow_data_cor)

# Summarize the model
summary(lm_depth_date)
summary(lm_depth_date_treat)
summary(lm_depth_date_treat_subs)

#plot(lm_depth_date_treat)
AIC(lm_depth_date,lm_depth_date_treat,lm_depth_date_treat_subs)

ggplot(snow_data_cor, aes(x=SNOW_DEPTH_CM, y=yearday, color=TREATMENT))+
  geom_point(aes(shape= SUBSITE))+
  geom_smooth(method="lm")+
  theme_bw()+
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1))+  # Rotate x-axis labels by 45 degrees)+
  scale_colour_viridis_d(
    option = "G",
    begin = 0,
    end = 0.8,
    aesthetics = "colour",
    name="Treatment"
  )+
  labs(shape = "Habitat") +  
  ylab("First snow free day (DOY)") +
  xlab("Snow depth in cm")
ggsave("./plots/env/snowmeltout_Depth.png",height=3.5, width=5)
