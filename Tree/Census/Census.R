# Reading data
census = read.csv("census.csv")
str(census)
# Split the data
library(caTools)
set.seed(2000)
split = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, split == T)
test = subset(census, split == F)
# Building logistic regression model
modelLog = glm(over50k ~., data = train, family = "binomial")
summary(modelLog)
# Predict the logistic regression model
pred = predict(modelLog, newdata = test, type = "response")
table(test$over50k, pred>0.5)
(9051+1888)/(9051+1888+662+1190)
# Acc = 0.855
# Baseline model
table(test$over50k)
9713/(9713+3078)
# AUC value
library(ROCR)
ROCRpred = prediction(pred, test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)
# CART model
library(rpart)
library(rpart.plot)
CARTmodel = rpart(over50k ~., data = train, method = "class")
prp(CARTmodel)
# ACC of the CART model
predCART = predict(CARTmodel, newdata = test, type = "class")
table(test$over50k, predCART)
(9243+1596)/(9243+470+1482+1596)
# ACC = 0.847
# A Random Forest Model
library(randomForest)
set.seed(1)
trainSmall = train[sample(nrow(train),2000),]
censusRF = randomForest(over50k ~., data = trainSmall)
predRF = predict(censusRF, newdata = test)
table(test$over50k, predRF)
(9614+1050)/(9614+1050+99+2028)
# ACC = 0.833
vu = varUsed(censusRF, count = TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(censusRF$forest$xlevels[vusorted$iX]))
# Selecting CP by cross validation
library(caret)
library(e1071)
numFolds = trainControl(method ="cv", number =10)
cpGrid = expand.grid( .cp = seq(0.002,0.1,0.0002))
train(over50k ~., data = train, method ="rpart", trControl = numFolds, tuneGrid = cpGrid)
censusCV = rpart(over50k ~., data = train, method = "class", cp = 0.0028)
predictCV = predict(censusCV, newdata = test, type = "class")
table(test$over50k, predictCV)
(9181+1837)/(9181+1837+1241+532)
# ACC = 0.86
prp(censusCV)
