# # --- Install Required Packages ---
# install.packages(c("cluster", "factoextra", "rattle.data"))
# 
# # --- Load Libraries ---
# library(cluster)
# library(factoextra)
# library(rattle.data)
# 
# # --- Load Wine Dataset ---
# data(wine, package = "rattle.data")
# 
# # Remove class label for unsupervised learning
# wine_data <- wine[, -1]
# 
# # Standardize the data
# wine_scaled <- scale(wine_data)
# 
# # --- Determine Optimal Number of Clusters (Elbow Method) ---
# fviz_nbclust(wine_scaled, kmeans, method = "wss") +
#   labs(subtitle = "Elbow Method for Optimal K")
# 
# # --- K-Means Clustering ---
# set.seed(123)
# k3 <- kmeans(wine_scaled, centers = 3, nstart = 25)
# print(k3)
# 
# # Visualize K-Means clusters
# fviz_cluster(k3, data = wine_scaled,
#              ellipse.type = "norm",
#              geom = "point",
#              main = "K-Means Clustering (k = 3)")
# 
# # --- Hierarchical Clustering ---
# d <- dist(wine_scaled, method = "euclidean")
# hc <- hclust(d, method = "ward.D2")
# 
# # Plot dendrogram
# plot(hc, cex = 0.6, hang = -1, main = "Dendrogram of Wine Data")
# 
# # Cut tree into 3 clusters
# hc_clusters <- cutree(hc, k = 3)
# 
# # Visualize hierarchical clusters
# fviz_cluster(list(data = wine_scaled, cluster = hc_clusters),
#              main = "Hierarchical Clustering on Wine Data")
# 
# # --- Compare Cluster Assignments ---
# table(KMeans = k3$cluster, Hierarchical = hc_clusters)
# 
# # --- Silhouette Analysis for K-Means ---
# fviz_silhouette(silhouette(k3$cluster, dist(wine_scaled)
#                            

           
                
# Load required packages
install.packages(c("cluster", "factoextra"))
library(cluster)
library(factoextra)
                           
# Load Wine dataset from UCI
wine <- read.csv(
"https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",
header = FALSE
)
                           
# Add column names
colnames(wine) <- c(
"Class",
"Alcohol",
"Malic_Acid",
"Ash",
"Alcalinity_of_Ash",
"Magnesium",
"Total_Phenols",
"Flavanoids",
"Nonflavanoid_Phenols",
"Proanthocyanins",
"Color_Intensity",
"Hue",
"OD280_OD315",
"Proline"
)
                           
# Remove class label for unsupervised learning
wine_data <- wine[, -1]
                         
# Standardize the data
wine_scaled <- scale(wine_data)
                       
# ---- Determine Optimal Number of Clusters ----
fviz_nbclust(wine_scaled, kmeans, method = "wss") +
labs(subtitle = "Elbow Method for Optimal K")
                           
# ---- Apply K-Means Clustering ----
set.seed(123)
k3 <- kmeans(wine_scaled, centers = 3, nstart = 25)
print(k3)
                           
# Visualize K-Means Clusters
fviz_cluster(
k3,
data = wine_scaled,
elipse.type = "norm",
geom = "point",
main = "K-Means Clustering on Wine Data"
)
                           
# ---- Apply Hierarchical Clustering ----
d <- dist(wine_scaled, method = "euclidean")
hc <- hclust(d, method = "ward.D2")
                       
 # Plot Dendrogram
plot(hc,
cex = 0.6,
hang = -1,
main = "Dendrogram of Wine Data")
         
 # Cut tree into 3 clusters
hc_clusters <- cutree(hc, k = 3)
                           
# Visualize Hierarchical Clusters
fviz_cluster(list(data = wine_scaled, cluster = hc_clusters), main = "Hierarchical Clustering on Wine Data")
                           
 # ---- Compare Cluster Assignments ----
table(KMeans = k3$cluster, Hierarchical = hc_clusters)
                           
# ---- Silhouette Analysis ----
fviz_silhouette(silhouette(k3$cluster, dist(wine_scaled)))