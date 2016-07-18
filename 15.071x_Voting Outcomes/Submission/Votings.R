# Loading data
train = read.csv("train2016.csv",na.strings = c("", "NA"))
test = read.csv("test2016.csv", na.strings = c("", "NA"))
str(train)
table(train$Party)
2951/(2951+2617)
mean(train$YOB, na.rm = T)
mean(test$YOB,na.rm = T)
train$YOB[train$YOB>2004 & !is.na(train$YOB)]
train$YOB[train$YOB<1901 & !is.na(train$YOB)]
test$YOB[test$YOB>2004 & !is.na(test$YOB)]
test$YOB[test$YOB<1901 & !is.na(test$YOB)]
# Looking high outlier
train[train$YOB>2004 & !is.na(train$YOB),]
test[test$YOB>2004 & !is.na(test$YOB),]
# Remove high outlier
train <- subset(train, train$YOB <= 2004 | is.na(train$YOB))
# Looking low outlier
train[train$YOB<1901 & !is.na(train$YOB),]
test[test$YOB<1901 & !is.na(test$YOB),]
# Combine data
trainParty = train$Party
train$Party = NULL
data = rbind(train,test)
# Create AgeGroup variable
AgeGroup = vector(mode = "character", length = 6956)
for(i in 1:6956){
  age = 2016 - data$YOB[i]
if(is.na(age)){
  AgeGroup[i] = NA
}
else if(age<18){
  AgeGroup[i] = "Under 18"
}
else if(age >= 18 & age < 25){
  AgeGroup[i] = "18-24"
}
else if(age >= 25 & age < 35){
  AgeGroup[i] = "25-34"
}
else if(age >= 35 & age < 45){
  AgeGroup[i] = "35-44"
}
else if(age >= 45 & age <55){
  AgeGroup[i] = "45-54"
}
else if(age >= 55 & age < 65){
  AgeGroup[i] = "55-64"
}
else if(age >= 65){
  AgeGroup[i] = "Over 65"
}
}
rm(i, age)
AgeGroup = factor(AgeGroup, c("Under 18", "18-24", "25-34", "35-44", "45-54", "55-64", "Over 65"), ordered = T)
data$YOB = NULL
USER_ID = data$USER_ID
data = cbind(USER_ID, AgeGroup, data[,2:106])
str(data)
rm(USER_ID, AgeGroup)
rm(data)
# Imputation to whole data
library(mice)
set.seed(1)
USER_ID = data[,1]
data = data[,2:107]
tempData = mice(data, m=2, maxit = 10)
fullData = complete(tempData)
data = cbind(fullData, USER_ID)
train = data[1:5564,]
test = data[5565:6956,]
train = cbind(train, trainParty)
rm(tempData, trainParty, USER_ID)
# Random Forest model
library(randomForest)
QuestionTrain = train[, 6:108]
Demographic = train[, 1:5]
modelRF = randomForest(trainParty ~. - USER_ID, data = QuestionTrain, ntree = 500, nodesize = 100)
pred = predict(modelRF, newdata = test)
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = pred)
write.csv(MySubmission, "ModelRF.csv", row.names =FALSE)
# ACC = 0.64224 (Rank 190 out of 2800)
# Logistic regression model
modelGlm = glm(trainParty ~. - USER_ID, data = QuestionTrain, family = binomial)
predGlm = predict(modelGlm, newdata = test, type = "response")
threshold = 0.5
PredTestLabels = as.factor(ifelse(predGlm<threshold, "Democrat", "Republican"))
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)
write.csv(MySubmission, "ModelLog.csv", row.names =FALSE)
# ACC = 0.61494
# Tree model
library(caret)
library(e1071)
numFolds = trainControl(method = "cv", number =10)
cpGrid = expand.grid( .cp =seq(0.002,0.01,0.002))
train(trainParty ~.-USER_ID, data=QuestionTrain, method ="rpart", trControl = numFolds, tuneGrid = cpGrid)
library(rpart)
library(rpart.plot)
modelTree = rpart(trainParty ~. - USER_ID, data = QuestionTrain, method = "class", cp = 0.01)
prp(modelTree)
predT = predict(modelTree, newdata = test, type ="class")
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = predT)
write.csv(MySubmission, "ModelTree.csv", row.names =FALSE)
# ACC = 0.61494
# See importance variables
varImpPlot(modelRF, sort = T, main = "Variable Importance", n.var = 20)
# Add AgeGroup to QuestionTrain data and use RandomForest to predict
AgeGroup = train$AgeGroup
QuestionTrain = cbind(QuestionTrain, AgeGroup) 
summary(QuestionTrain)
modelRF2 = randomForest(trainParty ~ Q109244 + Q115611 + Q98197 + Q113181 + Q98869 + Q120472 + Q101163 + Q110740 + Q105840 + Q100680 + Q99480 + Q106272 + Q121699 + Q120379 + Q120014 + Q96024 + Q115195 + Q115899 + Q116881 + Q119851 - USER_ID, data = QuestionTrain, ntree = 500, nodesize = 100)
predRF = predict(modelRF2, newdata = test)
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = predRF)
write.csv(MySubmission, "ModelRF2.csv", row.names =FALSE)
# ACC =0.6321


