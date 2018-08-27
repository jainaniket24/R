# Hierarchical Clustering

# importing the data
mall <- read.csv('Mall_Customers.csv')
# selecting the relevant features for clustering
mall <- mall[, 4:5]

# using the dendogram to determine optimal number of clusters
mall_dendogram = hclust(dist(mall, method = 'euclidean'), method = 'ward.D')
plot(mall_dendogram, main = 'Dendogram', xlab = 'Customers', ylab = 'Distances')

# from the dendogram, we observe that optimal number of cluster is 5
# fitting hierarchical clustering with 5 clusters
n_clust = 5
mall_clust <- cutree(tree = mall_dendogram, k = n_clust)

# plotting the clusters
library(cluster)
clusplot(x = mall, clus = mall_clust, lines = 0, shade = TRUE, color = TRUE, 
         labels = 2, span = TRUE, plotchar = FALSE, main = 'Clusters', 
         xlab = 'Annual Income', ylab = 'Score')