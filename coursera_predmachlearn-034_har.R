# ====================================================
# Course Project (predmachlearn-034)
# Activity Class Prediction Assignment
# Data source: http://groupware.les.inf.puc-rio.br/har
# Date: 2015-11-22
# Author: Siyu Sun (sunsiyu.tud@gmail.com)
# ====================================================

library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
set.seed(1122)

### Data Load ###
training <- read.csv("pml-training.csv", na.strings = c("#DIV/0!", "NA", "na"))[, -(1:5)]
testing <- read.csv("pml-testing.csv", na.strings = c("#DIV/0!", "NA", "na"))[, -(1:5)]
training <- tbl_df(training)
testing <- tbl_df(testing)
by_class <- group_by(training, classe)

### Data Clean ###
# Remove feature with NA percentage over 90%
per_na <- summarize_each(training, funs(sum(is.na(.))/nrow(training)))
per_na_var <- names(per_na[, per_na > 0.9])
training <- training[, !names(training) %in% per_na_var]
testing <- testing[, !names(testing) %in% per_na_var]
# Remove zero covariates
nsv <- nearZeroVar(training, saveMetrics = TRUE)
training <- training[, !names(training) %in% rownames(nsv[nsv$nzv,])]
testing <- testing[, !names(testing) %in% rownames(nsv[nsv$nzv,])]

### Preprocess ###
# PCA
prComp <- prcomp(training[, -54])
prePCA <- preProcess(training[, -54], method = "pca", pcaComp = 20)
training <- predict(prePCA, training[, -54])
testing <- predict(prePCA, testing[, -54])
# Standardization and no need for imputation
preObj <- preProcess(training[, -54], method = c("center", "scale"))
training <- predict(preObj, training[, -54])
testing <- predict(preObj, testing[, -54])
training$classe <- by_class$classe

### Train ###
inTrain <- createDataPartition(training$classe, p = 0.7, list = F)
trainset <- training[inTrain, ]
testset <- training[-inTrain, ]
# Try different models
# modFit1 <- train(classe ~ ., method = "glm", data = trainset)
# modFit2 <- train(classe ~ ., method = "lda", data = trainset)
# etc...

# Random Forest
class.rf <- randomForest(x = trainset[, -21], 
                         y = trainset[, 21], 
                         xtest = testset[, -21], 
                         ytest = testset[, 21], 
                         ntree = 100,
                         mtry = 5,
                         do.trace = 5,
                         keep.forest=TRUE)
# Check the model performance
print(class.rf)
table(class.rf$y, trainset[, 21])

### Predict ###
prediction <- predict(class.rf, newdata = testing)

### Evaluation ###
# print(modFit2)
print(class.rf)
table(class.rf$y, trainset[, 21])
table(prediction, testset[, 21])
