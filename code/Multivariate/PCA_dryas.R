####  Purpose of the code: run PCA for Dryas in 2022 dataset               ####
#     Author: Merle A. Scheiner  , 2024                                       #
#   needs: DryasPerPlot.csv                                                   # 
#   ToDo: fix the issures with the biplot                                     #
###############################################################################


setwd("~/Documents/Uni/Masterarbeit/Data_Analysis_Endalen")
library(tidyverse)
####Run PCA for the whole dataset of 2022####
#VASCULAR_wide <- write.csv("/Users/ms/Documents/Uni/Masterarbeit/Data_Analysis_Endalen/data/secondaryData/data2022PerPlot.csv", row.names = FALSE)
DRY_wide<- read.csv("DryasPerPlot.csv")
plots_dryl <-  c("DRY-L1",  "DRY-L10" , "DRY-L2"  ,"DRY-L3" , "DRY-L4" , "DRY-L5" , "DRY-L6" ,"DRY-L7","DRY-L8","DRY-L9")

DRY_2022 <- subset(DRY_wide, YEAR=="2022") %>% subset(., PLOT%in%plots_dryl)%>% #subset(., SUBSITE=="DRY-L")%>%
  discard(~all(is.na(.) | . ==""))

#DRY_2022 <- DRY_2022 %>% replace(is.na(.), 0)

str(DRY_2022)
vegdatapca.df <- as.data.frame(DRY_2022)
str(vegdatapca.df)
rownames(vegdatapca.df) <- vegdatapca.df$PLOT
head(vegdatapca.df)
summary(vegdatapca.df)

####simplePCA###
##calculate principal components
#results <- prcomp(t(vegdatapca.df[,-c(1:3)]), scale = TRUE)
##calculate principal components
##results <- prcomp(vegdatapca.df[,-c(1:3)], scale = TRUE)
#
##reverse the signs
#results$rotation <- -1*results$rotation
#
##display principal components
#results$rotation
#
#biplot(results, scale = 0)
#
#### fancy atempt####

head(vegdatapca.df)
#dry.pca.df<- subset(vegdatapca.df, SUBSITE=="DRY-L")
library('corrr')
library(ggcorrplot)
library("FactoMineR")
library(factoextra)

colSums(is.na(vegdatapca.df))
vegdatapca.df <- vegdatapca.df %>% replace(is.na(.), 0)

numerical_data <- t(vegdatapca.df[,-c(1:3)]) #analysies plot distrubtion
numerical_data <- (vegdatapca.df[,-c(1:3)]) # analyses species space
head(numerical_data)

#data_normalized <- scale(numerical_data)
#head(data_normalized)

#corr_matrix <- cor(data_normalized)
corr_matrix <- cor(numerical_data)
numerical_data
ggcorrplot(corr_matrix)

data.pca <- princomp(corr_matrix)
# if error: Error in cov.wt(z) : 'x' must contain finite values only
ndnew <- corr_matrix[complete.cases(corr_matrix),]
data.pca <- princomp(ndnew)
data.pca<- princomp(na.omit(corr_matrix), cor = TRUE)


summary(data.pca)

data.pca$loadings[, 1:2]

fviz_eig(data.pca, addlabels = TRUE)

# Graph of the variables
fviz_pca_var(data.pca, col.var = "black")
fviz_cos2(data.pca, choice = "var", axes = 1:2)


fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

fviz_pca_var(data.pca, geom = "point", col.var = "black")

# Graph of the variables (biplot without normalization and circle)
fviz_pca_var(data.pca, axes = c(1, 2), geom = "text", col.var = "black")

# Check dimensions of the dataset
dim(numerical_data)

# Transpose the dataset if necessary
numerical_data <- t(numerical_data)

# Check dimensions again
dim(numerical_data)

# Perform PCA without normalization
data.pca <- princomp(numerical_data)

# Summary of PCA results
summary(data.pca)

# Visualize the eigenvalues
fviz_eig(data.pca, addlabels = TRUE)

# Graph of the variables (biplot without normalization)
fviz_pca_var(data.pca, geom = "point", col.var = "black")


             