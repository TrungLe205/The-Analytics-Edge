colClasses = c("numeric")
names(colClasses) = c("YOB")
train = read.csv("train2016.csv", na.strings = "", colClasses = colClasses, stringsAsFactors = FALSE)
test = read.csv("test2016.csv", na.strings = "", colClasses = colClasses, stringsAsFactors = FALSE)
sum(is.na(train))
str(train)
# Use MICE packages dealing with missing data
library(mice)
imputed = complete(mice(train))
modelLog1 = glm(Party ~. - USER_ID, data = imputed, family= "binomial")
pred = predict(modelLog1, newdata = test, type = "response")
threshold = 0.5
PredTestLabels = as.factor(ifelse(pred<threshold, "Democrat", "Republican"))
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)
write.csv(MySubmission, "ModelLog1.csv", row.names =FALSE)
# ACC = 0.57902
# Tree
library(rpart)
library(rpart.plot)
modelTree2 = rpart(Party ~.-USER_ID, data = imputed, method = "class")
prp(modelTree2)
pred1 = predict(modelTree, newdata = test, type ="class")
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = pred1)
write.csv(MySubmission, "ModelTree2.csv", row.names =FALSE)
# ACC = 0.61207
# Cross Validation
library(caret)
library(e1071)
numFolds = trainControl(method = "cv", number =10)
cpGrid = expand.grid( .cp =seq(0.002,0.01,0.002))
train(Party ~.-USER_ID, data=train, method ="rpart", trControl = numFolds, tuneGrid = cpGrid)
modelTree1 = rpart(Party ~. -USER_ID, data = train, method = "class", cp =0.01)
pred2 = predict(modelTree1, newdata = test, type = "class")
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = pred2)
write.csv(MySubmission, "ModelTree1.csv", row.names =FALSE)
# ACC = 0.61207
# Random Forests
library(randomForest)
modelRF = randomForest(Party ~.-USER_ID, data = imputed, ntree = 200, nodesize = 50, cp = 0.04)
predRF = predict(modelRF, newdata = test)
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = predRF)
write.csv(MySubmission, "modelRF.csv", row.names =FALSE)
