# K-means Clustering

# importing the data
mall <- read.csv('Mall_Customers.csv')
x_var <- mall[, 4:5]

# using elbow method to find optimal number of clusters
set.seed(6)
wcss <- vector()
for(i in 1:10){
  wcss[i] <- sum(kmeans(x_var, i)$withinss)
}

plot(x = 1:10, y = wcss, type = 'b', main = 'Clusters',
     xlab = 'Number of Clusters', ylab = 'WCSS')

# we observe optimal value is 5
# applying K-means with 5 clusters
set.seed(29)
num_clust <- 5
mall_kmeans <- kmeans(x = x_var, centers = num_clust, iter.max = 300, nstart = 10)

# visualizing the clusters
library(cluster)
clusplot(x = x_var, clus = mall_kmeans$cluster, lines = 0, shade = TRUE, color = TRUE,
         labels = 2, plotchar = FALSE, span = TRUE, main = 'Cluster of Clients',
         xlab = 'Annual Income', ylab = 'Score')

