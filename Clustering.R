
# loading packages:
library(caret)
library(flexclust)

#### Question 1 ####

# Part 1
# loading the dataset
airlines <- read.csv('AirlinesCluster.csv')

summary(airlines)
# (a)
# variables with smallest average values: FlightTrans (1.374) and BonusTrans (11.6)

# (b)
# variables with largest average values: Balance (73601), BonusMiles (17145)

# (c)
# we normalise data because data on different scales can impact the inter-observation distances and hence the cluster that 
# each observation is categorized into

# (d)
preproc <- preProcess(airlines)
airlinesNorm <- predict(preproc, airlines)

summary(airlinesNorm)

# maximum value: FlightMiles (21.6803)

# (e)
# minimum value: DaysSinceEnroll (-1.99336)

# Part 2: Hierarchical Clustering
# (a)
distancesAirlines <- dist(airlinesNorm, method = "euclidean")
airlinesClust <- hclust(distancesAirlines, method = "ward.D")
plot(airlinesClust)

# between 2 and 10, 5 is the best choice as it has high wiggle room and hence clusters are robust

# (b)
clusterGroupsAirlines <- cutree(airlinesClust, k = 5)
table(clusterGroupsAirlines)[1]

# (c)
aggregate(airlines, by = list(clusterGroupsAirlines), mean)

# cluster 1 max value: DaysSinceEnroll
# Customers in cluster 1 are passive members. They have been members for longest but
# have least number of flight transactions on average and also lowest flight miles. They have
# very low bonus transactions and bonus miles and hence low overall balance.

# (d)
# cluster 2 max value: QualMiles, FlightMiles, FlightTrans
# Customers in cluster 2 are do flight transactions most frequently and hence have maximum
# flight miles as well. High QualMiles indicate that they do not frequently use miles for flights.
# These customers can be classified as frequent flier members. 

# (e)
# cluster 3 max value: Balance, BonusMiles, BonusTrans
# Customers in cluster 3 have high balance mostly contributed by bonus transactions. These
# customers avail bonus services more frequently as compared to other customers.

# (f)
# cluster 4 max value: none of the parameters
# Customers in cluster 4 are relatively new members with high frequency of bonus 
# transactions and hence high bonus miles per days since enrollment.

# (g)
# cluster 5 max value: none of the parameters
# Customers in cluster 5 are relatively new members that use flight services more frequently
# that other members of similar time of enrollment.

# Part 3: K-means Clustering

set.seed(88)
airlinesKmeans = kmeans(airlinesNorm, 5, iter.max = 1000)
table(airlinesKmeans$cluster)

# (a)
# 2 clusters (cluster 4 and 5) have more than 1000 observations

# (b)
aggregate(airlines, by = list(airlinesKmeans$cluster), mean)
# Clusters from k-means are different from hierarchical clustering. K-means clusters are more
# clearly separated (mean values in hierarchical clustering was much closer to each other as
# compared to K-means clustering).


#### Question 2 ####
dailykos <- read.csv('dailykos.csv')

# Part 1
# (a)
articleDistances <- dist(dailykos[,-1], method = 'euclidean')
articleClust <- hclust(articleDistances, method = 'ward.D')

# (b)
plot(articleClust)

# (c)
# For categorizing articles into clusters from which users can choose what type of articles to read, the 
# number should not be too high to confuse users at the same time it should not be too low to not
# properly distribute articles in various categories. Keeping this in mind, 5-7 clusters should be a good
# choice.

# (d)
clusterGroupsArticles <- cutree(articleClust, k = 7)
k <- 1:7

article1 <- subset(dailykos, clusterGroupsArticles == 1)
article2 <- subset(dailykos, clusterGroupsArticles == 2)
article3 <- subset(dailykos, clusterGroupsArticles == 3)
article4 <- subset(dailykos, clusterGroupsArticles == 4)
article5 <- subset(dailykos, clusterGroupsArticles == 5)
article6 <- subset(dailykos, clusterGroupsArticles == 6)
article7 <- subset(dailykos, clusterGroupsArticles == 7)

nrow(article3) # 374

articleList <- list(article1, article2, article3, article4, article5, article6, article7)

length(articleList[[3]]) # 1266

# (e)
which.max(table(clusterGroupsArticles)) # cluster 1
table(clusterGroupsArticles)[which.max(table(clusterGroupsArticles))]  # 1266 observations

# (f)
which.min(table(clusterGroupsArticles)) # cluster 4
table(clusterGroupsArticles)[which.min(table(clusterGroupsArticles))]  # 139 observations

# (g)
tail(sort(colMeans(article1[-1])))
# bush

# (h)
lapply(articleList, function(x) {tail(sort(colMeans(x[-1])))} )
# cluster 2: november, poll, vote, challenge

# (i)
# cluster 5 is most closely related to Iraq war

# (j)
# cluster 7 best corresponds to democratic party

# Part 2: K-means clustering
set.seed(1000)
articlesKmeans <- kmeans(dailykos[, -1], centers = 7)

articleKmeans1 <- subset(dailykos, articlesKmeans$cluster == 1)
articleKmeans2 <- subset(dailykos, articlesKmeans$cluster == 2)
articleKmeans3 <- subset(dailykos, articlesKmeans$cluster == 3)
articleKmeans4 <- subset(dailykos, articlesKmeans$cluster == 4)
articleKmeans5 <- subset(dailykos, articlesKmeans$cluster == 5)
articleKmeans6 <- subset(dailykos, articlesKmeans$cluster == 6)
articleKmeans7 <- subset(dailykos, articlesKmeans$cluster == 7)

articleKmeansList <- list(articleKmeans1, articleKmeans2, articleKmeans3, articleKmeans4, 
                          articleKmeans5, articleKmeans6, articleKmeans7)

# (a)
nrow(articleKmeans3) # 277

# (b)
maxKmeans <- which.max(table(articlesKmeans$cluster))
maxKmeans # 4
table(articlesKmeans$cluster)[maxKmeans] # 2063

# (c)
minKmeans <- which.min(table(articlesKmeans$cluster))
minKmeans # 2
table(articlesKmeans$cluster)[minKmeans] # 144

# (d)
lapply(articleKmeansList, function(x) {tail(sort(colMeans(x[-1])))} )
# cluster 3 best corresponds to Iraq war

# (e)
# cluster 2 best corresponds to democratic party

# (f)
# Hierarchical cluster 7 best corresponds to K-means cluster 2

# (g)
# Hierarchical cluster 5 best corresponds to K-means cluster 3

# (h)
# Hierarchical cluster 5 best corresponds to K-means cluster 1

# (i)
# Hierarchical cluster 2 best corresponds to K-means cluster 6

