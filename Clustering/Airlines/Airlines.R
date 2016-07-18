airlines = read.csv("AirlinesCluster.csv")
summary(airlines)
# We need to normalize 2 variables of BonusTrans and FlightTrans
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)
# Hierachical clustering
distances = dist(airlinesNorm, method = "euclidean")
clusterAirlines = hclust(distances, method = "ward.D")
plot(clusterAirlines)
# Group 5 clusters
clusterGroup = cutree(clusterAirlines, k=5)
cluster1 = subset(airlinesNorm, clusterGroup ==1)
cluster2 = subset(airlinesNorm, clusterGroup ==2)
cluster3 = subset(airlinesNorm, clusterGroup ==3)
cluster4 = subset(airlinesNorm, clusterGroup ==4)
cluster5 = subset(airlinesNorm, clusterGroup ==5)
airVec = c(tapply(airlines$Balance, clusterGroup, mean), tapply(airlines$QualMiles,clusterGroup, mean),tapply(airlines$BonusMiles, clusterGroup, mean), tapply(airlines$BonusTrans, clusterGroup, mean), tapply(airlines$FlightMiles,clusterGroup, mean), tapply(airlines$FlightTrans, clusterGroup, mean),tapply(airlines$DaysSinceEnroll, clusterGroup, mean))
dim(airVec) = c(5,7)
colnames(airVec) = c("Balance", "QualMiles", "BonusMiles", "BonusTrans", "FlightMiles", "FlightTrans", "DaysEnroll")
airVec


