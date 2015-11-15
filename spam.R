library(caret)
library(kernlab)
data(spam)  # spam email database from kernlab

# Split to training and testing datasets
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

# PCA
M <- abs(cor(training[, -58]))
diag(M) <- 0
which(M > 0.8, arr.ind = T)
names(spam)[c(34, 32)]
plot(spam[, 34], spam[, 32])
# weighted combination
X <- 0.71 * training$num415 + 0.71 * training$num857
Y <- 0.71 * training$num415 - 0.71 * training$num857
plot(X, Y)

# prcomp
smallSpam <- spam[, c(34, 32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[, 1], prComp$x[, 2])
prComp$rotation
# apply PCA on all SPAM data
typeColor <- ((spam$type == "spam") * 1 + 1)
prComp <- prcomp(log10(spam[, -58] + 1))
plot(prComp$x[, 1], prComp$x[, 2], col=typeColor, xlab="PC1", ylab="PC2")

preProc <- preProcess(log10(spam[, -58] + 1), method="pca", pcaComp = 2)
spamPC <- predict(preProc, log10(spam[, -58] + 1))
plot(spamPC[, 1], spamPC[, 2], col=typeColor)

# Preprocessing training dataset with PCA
preProc <- preProcess(log10(training[, -58] + 1), method = "pca", pcaComp = 2)
trainPC <- predict(preProc, log10(training[, -58] + 1))
# train training dataset(after preprocessing) with method glm
modelFit <- train(training$type ~ ., method = "glm", data = trainPC)
# testing dataset also needs preprocessing with PCA
testPC <- predict(preProc, log10(testing[, -58] + 1))
# confusion matrix for evaluation caret
confusionMatrix(testing$type, predict(modelFit, testPC))

# Alternative to preprocess-train-modelfit
modelFit <- train(training$type ~ ., method = "glm", preProcess="pca", data=training)
confusionMatrix(testing$type, predict(modelFit, testing))

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

