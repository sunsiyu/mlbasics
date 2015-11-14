library(caret)
library(kernlab)
data(spam)  # spam email database from kernlab

# Split training and testing dataset
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = F)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

ggplot(training, aes(capitalAve)) + geom_histogram()

# Normalization
# Z-score mean=0, sd=1
ntrainCapAve <- zscorenorm(training$capitalAve)
# default caret::preProcess
preObj <- preProcess(training[, -58], method = c("center", "scale"))
ntrainCapAve <- predict(preObj, training[, -58])$capitalAve
mean(ntrainCapAve)

ntestCapAve <- predict(preObj, testing[, -58])$capitalAve
mean(ntestCapAve)

# Apply preProcess directly in model fitting
modelFit <- train(type ~ ., data = training, preProcess=c("center", "scale"), method = "glm")
modelFit


# Imputing Data, how to handle NAs
preObj <- preProcess(training[, -58], method = "knnImpute")  # use knnImpute method



