# Load data
energy = read.csv("energy.csv", stringsAsFactors = F)
str(energy)
table(energy$GenTotalRenewable)
energy[energy$GenTotalRenewable == max(energy$GenTotalRenewable),]
summary(energy)
presidential.results = as.factor(energy$presidential.results)
tapply(energy$AllSourcesCO2, energy$presidential.results, mean, na.rm = T)
cor(energy$AllSourcesCO2, energy$EsalesIndustrial, use = "complete")
cor(energy$AllSourcesSO2, energy$EsalesIndustrial, use = "complete")
cor(energy$AllSourcesNOx, energy$EsalesIndustrial, use = "complete")
tapply(energy$EPriceTotal, energy$STATE, min)
tapply(energy$GenTotal, energy$STATE, max)
# Prediction of Model Solar Generation
set.seed(144)
spl = sample(1:nrow(energy), size = 0.7*nrow(energy))
train = energy[spl,]
test = energy[-spl,]
mod = glm(GenSolarBinary ~ GenHydro + GenSolar + CumlFinancial + CumlRegulatory + Total.salary + Import, data = train, family = "binomial")
summary(mod)
# Performance on the Test set
pred = predict(mod, newdata = test, type = "response")
table(test$GenSolarBinary, pred > 0.5)
(154+18)/(154+7+18+31)
# Clustering method
train.limited = train[,c('CumlRegulatory', 'CumlFinancial', 'presidential.results', 'Total.salary', 'Import')]
test.limited = test[,c('CumlRegulatory', 'CumlFinancial', 'presidential.results', 'Total.salary', 'Import')]
# Normalize train.limited & test.limited
preproc = preProcess(train.limited)
train.norm = predict(preproc, train.limited)
preproc1 = preProcess(test.limited)
test.norm = predict(preproc1,test.limited)
# K-means
set.seed = 144
km = kmeans(train.norm, centers = 2, iter.max = 1000)
install.packages("flexclust")
library(flexclust)
km.kcca = as.kcca(km, train.norm)
cluster.train = predict(km.kcca)
clusters = km$cluster
table(clusters)
table(cluster.train)
train1 = subset(train.norm, cluster.train ==1)
train2 = subset(train.norm, cluster.train ==2)
summary(train1)
summary(train2)
