library(caret)
library(kernlab)
data(spam)
# Split to training and testing set
inTrain <- createDataPartition(y=spam$type, p=0.75, list=F)
training <- spam[inTrain, ]  # 75%
testing <- spam[-inTrain, ]  # 25%

modelFit <- train(type~., data=training, method="glm")


set.seed(32323)
# K-fold for cross-validation, return training set
folds <- createFolds(y=spam$type, k=10, list=T, returnTrain=T)
# return testing set
folds <- createFolds(y=spam$type, k=10, list=T, returnTrain=F)
# Re-sampling
folds <- createResample(y=spam$type, times=10, list=T)
# Time Series
tme <- 1:1000
folds <- createTimeSlices(y=tme, initialWindow = 20, horizon = 10)
folds$train
folds$test

