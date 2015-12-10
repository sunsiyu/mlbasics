library(caret)
library(AppliedPredictiveModeling)
library(ElemStatLearn)
library(pgmm)
library(rpart)
library(randomForest)


data("airquality")
data(segmentationOriginal)
set.seed(125)

inTrain <- createDataPartition(segmentationOriginal$Case, p = 0.7, list = F)
training <- segmentationOriginal[inTrain, -1]
testing <- segmentationOriginal[-inTrain, -1]
modelFit <- train(Case ~ ., data = training, model="CART")
res <- predict()

ozone.rf <- randomForest(Ozone ~ ., data = airquality, mtry = 3, importance = T, na.action = na.omit)
print(ozone.rf)
