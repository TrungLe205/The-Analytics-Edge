setwd("~/Documents/DATA SCIENTIST/Projects/MIT Course/Logistic Regression/Parole")
parole = read.csv("parole.csv")
str(parole)
table(parole$violator)
library(caTools)
set.seed(144)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == T)
test = subset(parole, split == F)
train$state = as.factor(train$state)
train$crime = as.factor(train$crime)
model = glm(violator ~., data = train, family = binomial)
summary(model)
# Creating data frame of the individual and then predict the probability of this individual is a violator
theMan = data.frame(row.names = 1)
theMan$male = 1
theMan$race = 1
theMan$age = 1
theMan$state = as.factor(1)
theMan$time.served = 3
theMan$max.sentence = 12
theMan$multiple.offenses = 0
theMan$crime = as.factor(2)
theMan$violator = NA
# Probability of this individual is violator
prob = predict(model, newdata = theMan, type = "response")
prob
# Evaluate the model on the testing set
test$crime = as.factor(test$crime)
test$state = as.factor(test$state)
# Predicting the model on test set
pred = predict(model, newdata = test, type = "response")
table(test$violator, pred > 0.5)
# model's accuracy
179/202
# we should decrease the threshold as it will decrease the cases that people get violate but we predict not violate. 
table(test$violator, pred > 0.3)
# Using ROCR to find AUC value
library(ROCR)
ROCRpred = prediction (pred, test$violator)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
