# UNDERSTANDING RETAIL CONSUMERS

#In Unit 6, we saw how clustering can be used for market segmentation, the idea of 
# dividing airline passengers into small, more similar groups, and then designing a 
# marketing strategy specifically for each group.  In this problem, we'll see how this 
# idea can be applied to retail consumer data.

# In this problem, we'll use the dataset Households.csv, which contains data collected over 
# two years for a group of 2,500 households.  Each row (observation) in our dataset 
# represents a unique household.  The dataset contains the following variables:

# NumVisits = the number of times the household visited the retailer 
# AvgProdCount = the average number of products purchased per transaction
# AvgDiscount = the average discount per transaction from coupon usage (in %) - NOTE: Do not divide this value by 100!
# AvgSalesValue = the average sales value per transaction
# MorningPct = the percentage of visits in the morning (8am - 1:59pm)
# AfternoonPct = the percentage of visits in the afternoon (2pm - 7:59pm)

# Load data
HH = read.csv("Households.csv", stringsAsFactors = FALSE)
str(HH)
subset(HH, MorningPct >= 100)
subset(HH, AfternoonPct >= 100)
subset(HH, AvgSalesValue > 150, AvgDiscount)
subset(HH, AvgDiscount > 25 , AvgSalesValue)
Visit = subset(HH, NumVisits >= 300)
148/2500
# Normalizing the data
library(caret)
preproc = preProcess(HH)
HHNorm = predict(preproc, HH)
str(HHNorm)
max(HHNorm$NumVisits)
min(HHNorm$AfternoonPct)
set.seed(200)
distances = dist(HHNorm, method = "euclidean")
?hclust
ClusterShoppers = hclust(distances, method = "ward.D")
plot(ClusterShoppers, labels = F)
# K-mean Clustering
set.seed(200)
?kmeans
km = kmeans(HHNorm, centers = 10)
table(km$cluster)
clusters = km$cluster
table(clusters)
# K-mean with 5 clusters
set.seed(5000)
km2 = kmeans(HHNorm, centers = 5)
clusters1 = km2$cluster
table(clusters1)
