
####Run PCA for the whole dataset of 2022####
VASCULAR_wide <- read.csv("/Users/ms/Documents/Uni/Masterarbeit/Data_Analysis_Endalen/data/secondaryData/data2022PerPlot.csv")
VASCULAR_wide[,-ncol(VASCULAR_wide)]

str(VASCULAR_wide)
vegdatapca.df <- as.data.frame(VASCULAR_wide)
str(vegdatapca.df)
rownames(vegdatapca.df) <- vegdatapca.df$PLOT
head(vegdatapca.df)


####simplePCA###
#calculate principal components
results <- prcomp(t(vegdatapca.df[,-c(1:3)]), scale = TRUE)
#calculate principal components
results <- prcomp(vegdatapca.df[,-c(1:3)], scale = TRUE)

#reverse the signs
results$rotation <- -1*results$rotation

#display principal components
results$rotation

biplot(results, scale = 0)

#### fancy atempt####
head(vegdatapca.df)
dry.pca.df<- subset(vegdatapca.df, SUBSITE=="DRY-L")

library('corrr')
library(ggcorrplot)
library("FactoMineR")
library(factoextra)

colSums(is.na(dry.pca.df))

numerical_data <- t(dry.pca.df[,-c(1:3)])
numerical_data <- (dry.pca.df[,-c(1:3)])
head(numerical_data)

data_normalized <- scale(numerical_data)
head(data_normalized)

corr_matrix <- cor(numerical_data)
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


png("biplot.png", width = 800, height = 600)  # Set the dimensions as needed


fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

dev.off()  # Close the graphics device after saving
