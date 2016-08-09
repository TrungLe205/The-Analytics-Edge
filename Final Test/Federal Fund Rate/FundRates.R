# FORECASTING INTEREST RATE HIKES BY THE U.S. FEDERAL RESERVE

# The federal funds rate is the key interest rate that the U.S. Federal Reserve uses to 
# influence economic growth. The Federal Open Market Committee meets regularly to decide 
# whether to increase, decrease, or maintain the target interest rate. Their choice has 
# important ramifications that cascade through the economy, so the announcement of the 
# interest rates is eagerly awaited each month.

# In this problem, we will use analytics to try to predict when the Federal Reserve will 
# raise interest rates. We will look at monthly economic and political data dating back to 
# the mid-1960's.In this analysis, the dependent variable will be the binary outcome 
# variable RaisedFedFunds, which takes value 1 if the federal funds rate was increased 
# that month and 0 if it was lowered or stayed the same. For each month, the file 
# federalFundsRate.csv contains the following independent variables:

# Date: The date the change was announced.
# Chairman: The name of the Federal Reserve Chairman at the time the change was announced.
# PreviousRate: The federal funds rate in the prior month.
# Streak: The current streak of raising or not raising the rate, e.g. +8 indicates the rate has been increased 8 months in a row, whereas -3 indicates the rate has been lowered or stayed the same for 3 months in a row.
# GDP: The U.S. Gross Domestic Product, in Billions of Chained 2009 US Dollars.
# Unemployment: The unemployment rate in the U.S.
# CPI: The Consumer Price Index, an indicator of inflation, in the U.S.
# HomeownershipRate: The rate of homeownership in the U.S.
# DebtAsPctGDP: The U.S. national debt as a percentage of GDP
# DemocraticPres: Whether the sitting U.S. President is a Democrat (DemocraticPres=1) or a Republican (DemocraticPres=0)
# MonthsUntilElection: The number of remaining months until the next U.S. presidential election.

# Loading data
fedFunds = read.csv("federalFundsRate.csv", stringsAsFactors = FALSE)
str(fedFunds)
table(fedFunds$RaisedFedFunds)
291/(291+294)
table(fedFunds$RaisedFedFunds, fedFunds$Chairman)
# Split the data
set.seed(201)
library(caTools)
spl = sample.split(fedFunds$RaisedFedFunds,0.7)
training = subset(fedFunds, spl == T)
testing = subset(fedFunds, spl == F)
# Build logistic regression model
glm = glm(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data = training, family = binomial)
summary(glm)
# Test set prediction
pred = predict(glm, newdata = testing, type = "response")
table(testing$RaisedFedFunds, pred > 0.5)
60+57/(60+57+27+31)
# Compute AUC
library(ROCR)
ROCRpred = prediction(pred, testing$RaisedFedFunds)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
plot(ROCRperf, colorize=TRUE)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
# Cross validation
library(caret)
library(e1071)
numFolds = trainControl(method = "cv", number =10)
cpGrid = expand.grid(.cp = seq(0.001,0.01,0.001))
train(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data = training, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
# CART Tree
library(rpart)
library(rpart.plot)
Tree = rpart(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data = training, method = "class", cp = 0.016)
plot(Tree)
prp(Tree)
# Make prediction
pred1 = predict(Tree, newdata = testing, type = "class")
table(pred1, testing$RaisedFedFunds)
(64+48)/(64+48+40+23)
