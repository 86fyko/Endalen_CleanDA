#install.packages("tidyverse")
library("tidyverse")
setwd("~/Documents/Uni/Masterarbeit/Data_Analysis_Endalen")
treatment <- read.csv("./data/plot_treatments.csv")

### SNOW TRANSECT SNOWBED####
transect<- read.csv("./data/env/Snowbed_TRANSECT.csv")
#transect$SUBSITE <-
rep("BIS-H", nrow(transect)/2)
xx <- factor(sort(rank(row.names(transect))%%2))
levels(xx) = c("BIS-H", "BIS-L")
transect$SUBSITE <- xx

#ggplot(transect, aes(x= position, y=depth_cm))+
#  geom_point()+
#  geom_smooth(method="lm")

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
ggplot(allsnow, aes(x=SUBSITE, y=SNOW_MAST))+
  geom_point()


#bish<- c(mean(transect$depth_cm), "BIS-H")
####Snowdepthplot####
snowdepth.plot <- ggplot(allsnow, aes(x=SUBSITE, y=SNOW_DEPTH_CM, color=TREATMENT))+
  geom_boxplot()+
  geom_point(aes(x=SUBSITE,y=SNOW_MAST), color="black")+
  scale_colour_viridis_d(
    option = "G",
    aesthetics = "colour",
    begin = 0,
    end = 0.8,
    name="Treatment"
  )+
  #geom_point(aes(x=bish[2],y=as.numeric(bish[1])), color="black")+ #mean of the snowtransec
  #geom_boxplot(data=transect, aes(x=SUBSITE, y=depth_cm), color="grey")+
  coord_flip()+
  ylab("Snow depth in cm")+
  theme_bw()
ggsave(snowdepth.plot,"./plots/env/snow_SUBSITE.png", height=3.5, width=5)

lm_snow_site_tr<- lm(data=allsnow, SNOW_DEPTH_CM ~ SUBSITE + TREATMENT)
summary(lm_snow_site_tr)
AIC(lm_snow_site_tr)



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
ggsave("../plots/env/OTC_accumulation.png", height=3.5, width=5)

casl_snowdepthOTC <- lm(data=subset(OTC_long, SUBSITE=="CAS-L"), SNOW_DEPTH ~ POSITION)
summary(casl_snowdepthOTC)
dryl_snowdepthOTC <- lm(data=subset(OTC_long, SUBSITE=="DRY-L"), SNOW_DEPTH ~ POSITION)
summary(dryl_snowdepthOTC)
capture_casl_snowdepthOTC <- summary(dryl_snowdepthOTC)
capture.output(capture_casl_snowdepthOTC, file = "anova results casl_snowdepthOTC.txt")
#signifikant weniger schnee außerhalb der OTC auf den DRYAS sites.

####Snowmeltout####
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
  scale_colour_viridis_d(
    option = "G",
    begin = 0,
    end = 0.8,
    aesthetics = "colour",
    name=""
  )+
  ylab("Subsite")+
  xlab("Timing of Snowmelt")
snowmeltout.plot
ggsave(snowmeltout.plot,"./plots/env/snowmeltout_bp.png", height=3.5, width=5)




library("ggbump")
#install.packages("ggpmisc")
library(ggpmisc)
snowmelt_dryas<- snowmelt_long[snowmelt_long$SUBSITE==c("Dry-H","Dry-L"),]#
snowmelt_dryas$cover_prob <- as.numeric(snomelt_dryas$snow_cover/100)

ggplot(data=snowmelt_long, aes(x=as.numeric(date), y=snow_cover, color=SUBSITE))+
  geom_point()+
  #  geom_smooth(method = "glm", formula ="binomial")+
  #  geom_smooth(method = "nls",
  #              formula = y ~ SSlogis(x, Asym, xmid, scal),
  #              se = FALSE)+
  geom_smooth(method="loess")+
  ##  stat_fit_tidy(method = "nls", formula =  y ~ log(x/(1-x)), #ggpmisc version
  #                aes(color = "sigmoid fit"))+
  #  geom_sigmoid()+
  theme_bw()

all_snow.df<- merge(snowmelt_long, allsnow, by="SUBSITE", all=T)

ggplot(all_snow.df, aes(x=date, y=snow_cover, color=SNOW_DEPTH_CM, group=SUBSITE))+
  geom_jitter()+
  geom_smooth(method="loess", aes(linetype=SUBSITE))+
  theme_bw()

ggplot(all_snow.df, aes(x=date, y=snow_cover, color=SNOW_DEPTH_CM, group=TREATMENT))+
  geom_jitter()+
  geom_smooth(method="loess", aes(linetype=TREATMENT))+
  theme_bw()


m1<- lm(data=all_snow.df, snow_cover ~ SNOW_DEPTH_CM)
summary(m1)

m2<- lm(data=all_snow.df, snow_cover ~ SNOW_DEPTH_CM + date)
summary(m2)

m1<- lm(data=all_snow.df, snow_cover ~ TREATMENT)
summary(m1)


m3<- lm(data=all_snow.df[all_snow.df$SUBSITE=="DRY-L",], snow_cover ~ date+TREATMENT)
summary(m3)

AIC(m1,m2)

ggplot(subset(all_snow.df, date==unique(all_snow.df$date)[3:12]), aes(x=SNOW_DEPTH_CM, y=snow_cover))+
  geom_jitter(aes(color=SUBSITE))+
  geom_smooth(method="lm")+
  theme_bw()


ggplot(data, aes(x = date, y = snow_cover, group = subsite, color = Site)) +
  geom_point() +
  #geom_line(aes(y = predicted_snow_cover), linetype = "dashed") +
  labs(x = "Date", y = "Snow Cover (%)", title = "Snow Cover Trend") +
  theme_minimal()

m1<- lm(data=snowmelt_long, snow_cover ~ date + SUBSITE)
summary(m1)
m1<- lm(data=snowmelt_long, snow_cover ~ SUBSITE)
summary(m1)

mean(subset(allsnow,SUBSITE=="CAS-L")$SNOW_DEPTH_CM)


#### overlap snowdepth and meltout####
plot_data$yearday <-  as.numeric(yday(plot_data$FirstZeroDate))
plot <- ggplot(data = plot_data, aes(x = SUBSITE.y, y = yearday)) +
  geom_boxplot() +
  ylab("Date")+
  theme_bw()
plot
# Create a secondary y-axis for SNOW_DEPTH_CM
plot <- plot + geom_col(aes(y = SNOW_DEPTH_CM), fill = "darkred", alpha=0.5) +
  stat_summary(
    fun.data = mean_se, 
    geom = "errorbar", 
    #position = position_dodge(width = 0.75),  # Match the position with the columns
    width = 0.3  # Adjust the width for better alignment
  ) +
  scale_y_continuous(
    name = "Date",
    sec.axis = sec_axis(~., name = "SNOW_DEPTH_CM")
  )
# Print the plot
print(plot)

snowmeltout.plot<- ggplot(data=result_data, aes(x=as_date(FirstZeroDate), y=SUBSITE, color=TREATMENT))+
  geom_boxplot()+
  theme_bw()+
  scale_colour_viridis_d(
    option = "G",
    begin = 0,
    end = 0.8,
    aesthetics = "colour",
    name=""
  )+
  ylab("Subsite")+
  xlab("Timing of Snowmelt")

snowdepth.plot <- ggplot(allsnow, aes(x=SUBSITE, y=SNOW_DEPTH_CM))+
  geom_col(position = position_dodge(width = 0.75)) +
  stat_summary(
    fun.data = mean_se, 
    geom = "errorbar", 
    position = position_dodge(width = 0.75),  # Match the position with the columns
    width = 0.3  # Adjust the width for better alignment
  ) +
  geom_point(aes(x=SUBSITE,y=SNOW_MAST), color="black")+
  scale_colour_viridis_d(
    option = "G",
    aesthetics = "colour",
    begin = 0,
    end = 0.8,
    name="Treatment"
  )+
  #geom_point(aes(x=bish[2],y=as.numeric(bish[1])), color="black")+ #mean of the snowtransec
  #geom_boxplot(data=transect, aes(x=SUBSITE, y=depth_cm), color="grey")+
  coord_flip()+
  ylab("Snow depth in cm")+
  theme_bw()

snowdepth.plot <- ggplot(allsnow, aes(x = SUBSITE, y = SNOW_DEPTH_CM)) +
  geom_col(position='dodge') +  # Set the fill color to dark red
  #  geom_point(aes(x = SUBSITE, y = SNOW_MAST), color = "black") +
  stat_summary(
    fun.data = mean_se, 
    geom = "errorbar", 
    #position = position_dodge(width = 0.75),  # Match the position with the columns
    width = 0.3  # Adjust the width for better alignment
  ) +
  #  scale_color_manual(values = "darkred") +  # Set the line color to dark red
  coord_flip() +
  ylab("Snow depth in cm") +
  theme_bw()
snowdepth.plot
