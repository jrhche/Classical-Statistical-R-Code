# Set working Directory
setwd("D:/Projects/Folders/Projects-Backup/GSU/Machine Learning/")

#Import libraries
library(devtools)
install_github("ggbiplot", "vqv")
library(ggbiplot)
library(factoextra)

rm(list = ls()) 
graphics.off()

# Read dioxin sample data data file
data = read.csv('dioxindata3.csv', header=TRUE)
rownames(data) <- data$Sample
data$Sample <- NULL

# Correlation Matrix
round(cor(data[,-1]), 2)

# PCA Analysis for correlation matrix
dioxinpca <-prcomp(data[,-1], scale. =TRUE) # shouldn't the variance be scaled/normalized to 1
summary(dioxinpca)

# Plot PCs vs Variance to determine how PCs needed to explan variation
plot(dioxinpca, type = "l") # 95% OF VARIANCE IS COVERED BY PC1 AND PC2, THUS WILL USE 2D PLOT
fviz_eig(dioxinpca)
fviz_eig(dioxinpca, addlabels = TRUE, ylim = c(0, 100))

# biplot of observations full
biplot(dioxinpca, scale = 1)

# create data frame with eigenvectors
dioxinpca$rotation
scores <- as.data.frame(dioxinpca$rotation)
Groups <- read.csv('dioxindata.csv', header=TRUE)
Groups <- as.factor(data$Group)
scores <-cbind(scores,Groups)

# plot of observations full
ggplot(data = scores, aes(x = PC1, y = PC2, colour=Group,label = rownames(scores))) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_point(data = scores, aes(PC1, PC2), size = 1)+
  geom_text(alpha = 0.8, size = 4, check_overlap = TRUE) +
  ggtitle("PCA plot of Dioxin Congeners - Samples")

# A represents secondary smelter baghouse dust samples (source)
# B represents anthropogenic background
# C represents mixture of sediments/soils with baghouse dust

fviz_pca_ind(dioxinpca,
             geom.ind = "point", # show points only (nbut not "text")
             #col.ind = Groups, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)

fviz_pca_ind(dioxinpca,col.var = grp,
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Groups")

fviz_pca_var(dioxinpca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(dioxinpca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)


# Eigenvalues
eig.val <- get_eigenvalue(dioxinpca)
eig.val

# Results for Variables
res.var <- get_pca_var(dioxinpca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(dioxinpca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 

