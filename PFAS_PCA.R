# Set working Directory
setwd("C:/Users/jhenderson/OneDrive - Formation Environmental LLC/Projects/Parris_MtVernon_Mills/R Code/")

#Import libraries
install.packages("devtools")
install_github("ggbiplot", "vqv")
library(ggbiplot)
library(factoextra)
suppressPackageStartupMessages(library(ggplot2))

rm(list = ls()) 
graphics.off()

# Read dioxin sample data data file
data = read.csv('PFASpcainput3.csv', header=TRUE)
rownames(data) <- data$Sample
data$Sample <- NULL

# Correlation Matrix
round(cor(data[,-1]), 2)

# PCA Analysis for correlation matrix
dioxinpca <-prcomp(data[,-1], scale. =FALSE) # shouldn't the variance be scaled/normalized to 1
summary(dioxinpca)

# Plot PCs vs Variance to determine how PCs needed to explain variation
plot(dioxinpca, type = "l") # 95% OF VARIANCE IS COVERED BY PC1 AND PC2, THUS WILL USE 2D PLOT
fviz_eig(dioxinpca)
fviz_eig(dioxinpca, addlabels = TRUE, ylim = c(0, 100))

# biplot of observations full
biplot(dioxinpca, scale = 1)

# create data frame with eigenvectors
scores = as.data.frame(dioxinpca$x)
Groups <- data[,1]


# plot of observations full
ggplot(data = scores, aes(x = PC1, y = PC2, color = Groups,label = Groups)) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_point(data = scores, aes(PC1, PC2), size = 1)+
  geom_text(alpha = 0.8, size = 2, check_overlap = TRUE) +
  ggtitle("PCA plot of PFAS Congeners - Samples")

# A represents secondary smelter baghouse dust samples (source)
# B represents anthropogenic background
# C represents mixture of sediments/soils with baghouse dust


ggplot(data = scores, aes(x = PC1, y = PC2, colour = Groups,label = Groups)) + 
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_point(data = scores, aes(PC1, PC2), size = 2)+
  geom_text(alpha = 0.8, size = 4) + xlim(500, -1500) +  ylim(-250, 250)















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

