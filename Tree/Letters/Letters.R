letters = read.csv("letters_ABPR.csv")
letters$isB = as.factor(letters$letter == "B")
str(letters)
# Spliting the data
library(caTools)
split = sample.split(letters, SplitRatio = 0.5)
train = subset(letters, split == T)
test = subset(letters, split == F)
# Baseline model
table(test$isB)
377/(1182+377)
# Building logistic regression model
modelLog = glm(isB ~. - letter, data = train, family = "binomial")
summary(modelLog)
# Warning message: glm.fit: fitted probabilities numerically 0 or 1 occurred 
# Using bayesian to solve the problem of perfect seperation
install.packages("arm")
library(arm)
modelLog = bayesglm(isB ~. - letter, data = train, family = "binomial")
summary(modelLog)
# Predicting model
pred = predict(modelLog, newdata = test, type = "response")
summary(pred)
# Confusing matrix with threshold of 0.5
table(test$isB, pred > 0.5)
(1146+335)/(1146+335+36+42)
# Acc = 94.9%
library(ROCR)
ROCRpred = prediction(pred, test$isB)
ROCRperf = performance ( ROCRpred, "tpr", "fpr")
plot(ROCRperf)
# Build classification tree to predict this model
library(rpart)
library(rpart.plot)
tree = rpart(isB ~. - letter, data = train, method = "class")
prp(tree)
predTree = predict(tree, newdata = test, type = "class")
table(predTree, test$isB)
(1134+317)/(1134+69+48+317)
# Acc = 92.5%
# Using Random Forest
install.packages("randomForest")
randomForest = randomForest(isB ~. - letter, data = train, ntree = 200)
predRF = predict(randomForest, newdata = test)
table(predRF, test$isB)
(1169+362)/(1169+15+13+362)
# Acc = 98.2%
# Using cross=validation to find cp to get better model
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)
numFolds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid( .cp =seq(0.01,0.5,0.01))
train(isB ~. - letter, data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
treeModel = rpart(isB ~. - letter, data = train, method ="class", cp = 0.01) 
predModel = predict(treeModel, newdata = test, type = "class")
table(test$isB, predModel)
(1134+317)/(1134+48+60+317)
# Acc = 93%