songs = read.csv("songs.csv")
str(songs)
songsFrom2009 = subset(songs, year >=2009)
# How many songs from 2009
str(songsFrom2009)
# How many songs that Eminem has
Eminem = subset(songs, artistname == "Eminem")
nrow(Eminem)
str(Eminem)
# Which songs of Eminem in top 10 ?
EminemTop10 = subset(Eminem, Top10 == 1)
EminemTop10$songtitle
# What timesignature is the most frequent ?
table(songs$timesignature)
# Building and Predicting model
# Split data into training and testing data
train = subset(songs, year <2009 )
test = subset(songs, year >= 2009)
nonvar = c("year", "songtitle", "artistname", "songID", "artistID")
# Remove nonvar from training and testing set
Train = train[ , !(names(train) %in% nonvar)]
Test = test [ , !(names(test) %in% nonvar)]
str(Train)
model = glm(Top10 ~ ., data = Train, family = "binomial")
summary(model)
cor(Train)
cor(Train$loudness, Train$energy)
# As these two variables are highly correlated, the model suffer multicollinearity,
# To avoid this issue, we need to omit one variable and rerun the model
model1 = glm(Top10 ~. - loudness, data = Train, family = "binomial")
summary(model1)
model2 = glm(Top10 ~. - energy, data = Train, family = "binomial")
summary(model2)
# Validating model
pred = predict(model2, newdata = Test, type = "response")
table(Test$Top10, pred > 0.5)
(726+20)/(726+14+96+20)
library(ROCR)
ROCRpred = prediction (pred, Test$Top10)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
plot(ROCRperf, colorize = TRUE)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


