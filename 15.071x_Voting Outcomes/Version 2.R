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
# 