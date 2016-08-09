# Load ggplot2, maps, ggmap
library(ggplot2)
library(maps)
library(ggmap)
# Load US map
statesMap = map_data("state")
statesMap
str(statesMap)
table(statesMap$group)
# Draw maps of US
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")
# Load data Polling
polling = read.csv("PollingImputed.csv")
str(polling)
# Split data
train = subset(polling, Year == 2004 | Year == 2008)
test = subset(polling, Year == 2012)
# Build logistic model
mod2 = glm(Republican ~ SurveyUSA + DiffCount, data = train, family = "binomial")
TestPrediction = predict(mod2, newdata = test, type = "response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, test$State)
# States prediction 1 (Republican)
table(TestPredictionBinary)
22/45
# Lower case Test.State
predictionDataFrame$region = tolower(predictionDataFrame$test.State)
# Merge statesMap and predictionDataFrame
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]
# Color the map
ggplot(predictionMap, aes(x = long, y= lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
