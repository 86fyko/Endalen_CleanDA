#library(glmm)
library(tidyverse)
data_clean <- read.csv("./project work/Data_Vasc_species_clean.csv")
snowdepth <- read.csv("./project work/Plant project snowdepth data ab-325_825 - Blad2.csv",dec = "," )
all.data_clean <- merge(data_clean,snowdepth,  by.x = c("Grid", "Row","Col"), by.y=c("Grid", "Row","Column"))
all_long_cleaned <- pivot_longer(all.data_clean, cols=c(7:57), names_to="Species", values_to="Cover")



######################
#########lm <- lm(Cover~as.numeric(Snowdepth..cm.)+Species,all_long_cleaned)
#############summary(lm)
#############plot(lm)
library(lme4)
library(nlme)
library(broom)

sub_spec_only<- subset(all_long_cleaned, Cover !=0)

m1<- glm(as.numeric(Cover)~as.numeric(Snowdepth..cm.)+Species, sub_spec_only, family = Gamma)
#m1<- glm(Species~as.numeric(Snowdepth..cm.), all_long_cleaned, family = quasipoisson)
#plot(m1)
write.csv(tidy(m1), "./model output/all_grids.csv", row.names=F)

models <- list()
grid_names <- levels(as.factor(sub_spec_only$Grid))
for(i in 1:4){
subset <- subset(sub_spec_only, Grid= grid_names[i])
models[[i]]<- glm(as.numeric(Cover)~as.numeric(Snowdepth..cm.)+Species, subset, family = Gamma) #creates one model for each grid
#m1<- glm(Species~as.numeric(Snowdepth..cm.), all_long_cleaned, family = quasipoisson)
write.csv(tidy(models[[i]]), paste("./model output/", grid_names[i], ".csv", sep=""), row.names=F) #model output
#summary(m1)
#plot(m1)
}




ggplot(sub_spec_only, aes(x=Cover, y= as.numeric(Snowdepth..cm.), color=Species))+
  geom_col(position = "dodge")+
  coord_flip()+
  theme_bw()+
  facet_grid(cols = vars(Grid))

sub_spec_only$Species <- gsub("Alopecurus_magellanicus","Alopecurus_ovatus", sub_spec_only$Species)
fun_group <- read.csv("./project work/species_list.csv")
fungroup_present<- merge(sub_spec_only, fun_group[,-3])

m2<- glm(as.numeric(Cover)/100~as.numeric(Snowdepth..cm.)+functional_group, fungroup_present, family = binomial())
#m1<- glm(Species~as.numeric(Snowdepth..cm.), all_long_cleaned, family = quasipoisson)
summary(m2)
plot(m2)
#write.csv(tidy(m2), "./model output/all_grids.csv", row.names=F)



Species_levels<- levels(as.factor(fungroup_present$Species))
labels <- gsub("_", " ", Species_levels)
paste(labels, "*")
newlables <- (as.data.frame(cbind(Species_levels, paste(labels, "*"))))


#fungroup_present<- merge(sub_spec_only, fun_group[,-3], all=T)
glm_output <- read.csv( "./model output/all_grids.csv")
significant_species.df<- subset(glm_output, p.value < 0.01)
significant_species_r  <- gsub("Species", "", significant_species.df$term)
significant_species <- significant_species_r[-1]
fungroup_present$Species2 <- gsub("_", " ", fungroup_present$Species) %>%   paste0(.," *")



non_significant_species.df<- subset(glm_output, p.value >= 0.05)
non_significant_species_r  <- gsub("Species", "", non_significant_species.df$term)
non_significant_species <- non_significant_species_r[-1]
non_significant_species <- gsub("_", " ", non_significant_species)

fungroup_present$Labels <- fungroup_present$Species2
for(i in 1:length(as.factor(non_significant_species))){
  fungroup_present$Labels <- gsub(paste(non_significant_species[i] ,"*"),paste(non_significant_species[i], "  "),fungroup_present$Labels, fixed=T)
}

#fungroup_present$Labels <- gsub("Carex rupestris *","Carex rupestris  ",fungroup_present$Species2, fixed=T)
#fungroup_present$Labels <- gsub("Carex rupestris *","Carex rupestris  ",fungroup_present$Species2, fixed=T)

#fungroup_present$Labels <- fungroup_present$Species2
#spec_names <- levels(as.factor(fungroup_present$Species))
#for(i in 1:length(spec_names)){
#  if(spec_names[i] %in% significant_species){
#  replacement <- paste(spec_names[i],"*")
#  fungroup_present$Labels <- gsub(spec_names[i], replacement, fungroup_present$Species, fixed=T)}
#  else{
#  replacement <- paste(spec_names[i]," ")
#  fungroup_present$Labels <- gsub(spec_names[i], replacement, fungroup_present$Species, fixed=T)}
#}



fungroup_present$functional_group2 <-  gsub("dwarf_shrub", "dwarf- \nshrub",fungroup_present$functional_group)
fungroup_present$functional_group2 <-  gsub("grass", "graminoid",fungroup_present$functional_group2)

#colorscale <- c("#006874" , "#7F7565","#9CA799") #"#6F7B66") #"#9CA799" #"#3F552A" #"#90A2A5"
colorscale <- c( "#7F7565" , "darkgreen","lightblue")
ggplot(fungroup_present, aes(y= as.numeric(Snowdepth..cm.), x=reorder(Labels,as.numeric(Snowdepth..cm.)), fill=functional_group2))+#, color=functional_group))+
  geom_boxplot()+
  facet_grid(row= vars(functional_group2),scales = "free", space = "free")+
  coord_flip()+
  theme_minimal()+
  theme(strip.text.y = element_text(angle=0))+
  guides(fill = "none")+ 
  ylab("Snowdepth [cm]")+
  scale_fill_manual(values=colorscale)+
  #scale_x_discrete(labels = paste(labels, "*"))+
  xlab("")
  #facet_grid(cols = vars(Grid))
ggsave("boxplot_species_snow.png", dpi=600, height=17, width=15, unit="cm" )


ggplot(subset(fungroup_present, Grid=="SVA1"), aes(y= as.numeric(Snowdepth..cm.), x=reorder(Labels,as.numeric(Snowdepth..cm.)), fill=functional_group2))+#, color=functional_group))+
  geom_boxplot()+
  facet_grid(row= vars(functional_group2),scales = "free", space = "free")+
  coord_flip()+
  theme_minimal()+
  theme(strip.text.y = element_text(angle=0))+
  guides(fill = "none")+ 
  ylab("Snowdepth [cm]")+
  scale_fill_manual(values=colorscale)+
  #scale_x_discrete(labels = paste(labels, "*"))+
  xlab("")
#facet_grid(cols = vars(Grid))
ggsave("boxplot_species_snow_SVA1.png", dpi=600, height=17, width=15, unit="cm" )

ggplot(subset(fungroup_present, Grid=="SVA2"), aes(y= as.numeric(Snowdepth..cm.), x=reorder(Labels,as.numeric(Snowdepth..cm.)), fill=functional_group2))+#, color=functional_group))+
  geom_boxplot()+
  facet_grid(row= vars(functional_group2),scales = "free", space = "free")+
  coord_flip()+
  theme_minimal()+
  theme(strip.text.y = element_text(angle=0))+
  guides(fill = "none")+ 
  ylab("Snowdepth [cm]")+
  scale_fill_manual(values=colorscale)+
  #scale_x_discrete(labels = paste(labels, "*"))+
  xlab("")
#facet_grid(cols = vars(Grid))
ggsave("boxplot_species_snow_SVA2.png", dpi=600, height=17, width=15, unit="cm" )

ggplot(subset(fungroup_present, Grid=="SVA5"), aes(y= as.numeric(Snowdepth..cm.), x=reorder(Labels,as.numeric(Snowdepth..cm.)), fill=functional_group2))+#, color=functional_group))+
  geom_boxplot()+
  facet_grid(row= vars(functional_group2),scales = "free", space = "free")+
  coord_flip()+
  theme_minimal()+
  theme(strip.text.y = element_text(angle=0))+
  guides(fill = "none")+ 
  ylab("Snowdepth [cm]")+
  scale_fill_manual(values=colorscale)+
  #scale_x_discrete(labels = paste(labels, "*"))+
  xlab("")
#facet_grid(cols = vars(Grid))
ggsave("boxplot_species_snow_SVA5.png", dpi=600, height=17, width=15, unit="cm" )




ggplot(subset(fungroup_present, Grid=="SVA6"), aes(y= as.numeric(Snowdepth..cm.), x=reorder(Labels,as.numeric(Snowdepth..cm.)), fill=functional_group2))+#, color=functional_group))+
  geom_boxplot()+
  facet_grid(row= vars(functional_group2),scales = "free", space = "free")+
  coord_flip()+
  theme_minimal()+
  theme(strip.text.y = element_text(angle=0))+
  guides(fill = "none")+ 
  ylab("Snowdepth [cm]")+
  scale_fill_manual(values=colorscale)+
  #scale_x_discrete(labels = paste(labels, "*"))+
  xlab("")
#facet_grid(cols = vars(Grid))
ggsave("boxplot_species_snow_SVA6.png", dpi=600, height=17, width=15, unit="cm" )


Species_levels<- cbind(levels(as.factor(fungroup_present$Species)), 
                       rep("*", length(levels(as.factor(fungroup_present$Species)))))

Species_levels<- levels(as.factor(fungroup_present$Species))
labels <- gsub("_", " ", Species_levels)
paste(labels, "*")
newlables <- (as.data.frame(cbind(Species_levels, paste(labels, "*"))))

ggplot(fungroup_present, aes(y= as.numeric(Snowdepth..cm.), x=reorder(functional_group,as.numeric(Snowdepth..cm.)), color=functional_group, fill=functional_group))+
  geom_boxplot()+
  #  facet_grid(row= vars(functional_group))+
  coord_flip()+
  theme_bw()



#mixed.model <- lme(Cover~as.numeric(Snowdepth..cm.), random=~1|Grid,all_long_cleaned)
#summary(mixed.model)
#anova(mixed.model)
#plot(fitted(mixed.model),resid(mixed.model))
#qqnorm(resid(mixed.model))
#qqnorm(ranef(mixed.model)[,1])
#plot(as.numeric(Snowdepth..cm.),resid(mixed.model))
#plot(mixed.model)
#
#
#
#mixed.model <- lme(Cover~as.numeric(Snowdepth..cm.), random=~1|Grid,all_long_cleaned)
#summary(mixed.model)
#anova(mixed.model)
#plot(fitted(mixed.model),resid(mixed.model))
#qqnorm(resid(mixed.model))
#qqnorm(ranef(mixed.model)[,1])
#plot(as.numeric(Snowdepth..cm.),resid(mixed.model))
#plot(mixed.model)

#mod_lmer2<-lmer(Cover~as.numeric(Snowdepth..cm.)+(as.numeric(Snowdepth..cm.)|Grid),data=all_long_cleaned)
