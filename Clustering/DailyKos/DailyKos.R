# Read the data
dailyKos = read.csv("dailykos.csv")
# Compute the distance
dist = dist(dailyKos, method = "euclidean")
# Hierachical clustering
clusterDailyKos = hclust(dist, method = "ward.D")
plot(clusterDailyKos)
# which of the following seem like good choices for the number of clusters?
# Pick the one that give you the longest distance between the children's node and the parent's node
# We do not want the category to be so general, so we pick the number of cluster based on that metric
# Let's pick 7 clusters. Use the cutree function to split your data into 7 clusters, create 
# 7 new datasets, each containing the observations from one of the clusters. 
clusterGroups = cutree(clusterDailyKos, k = 7)
cluster1 = subset(dailyKos, clusterGroups==1)
cluster2 = subset(dailyKos, clusterGroups==2)
cluster3 = subset(dailyKos, clusterGroups==3)
cluster4 = subset(dailyKos, clusterGroups==4)
cluster5 = subset(dailyKos, clusterGroups==5)
cluster6 = subset(dailyKos, clusterGroups==6)
cluster7 = subset(dailyKos, clusterGroups==7)
str(cluster1)
# The most frequently words appear in each cluster
tail(sort(colMeans(cluster1)))
tail(sort(colMeans(cluster2)))
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))
tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))

# K-means clustering
set.seed(1)
kmeansClustering = kmeans(dailyKos, 7)
str(kmeansClustering)
table(kmeansClustering$cluster)
kcluster1 = subset(dailyKos, kmeansClustering$cluster == 1)
kcluster2 = subset(dailyKos, kmeansClustering$cluster == 2)
kcluster3 = subset(dailyKos, kmeansClustering$cluster == 3)
kcluster4 = subset(dailyKos, kmeansClustering$cluster == 4)
kcluster5 = subset(dailyKos, kmeansClustering$cluster == 5)
kcluster6 = subset(dailyKos, kmeansClustering$cluster == 6)
kcluster7 = subset(dailyKos, kmeansClustering$cluster == 7)
str(kcluster7)
tail(sort(colMeans(kcluster1)))
tail(sort(colMeans(kcluster2)))
tail(sort(colMeans(kcluster3)))
tail(sort(colMeans(kcluster4)))
tail(sort(colMeans(kcluster5)))
tail(sort(colMeans(kcluster6)))
tail(sort(colMeans(kcluster7)))

