# ============================================================================
# This script with only be about classification.
# 2016-01-07
# Author: Siyu Sun
# ============================================================================

library(caret)
library(e1071)

set.seed(17)

# dataset: spambase (UCI ML repo downloaded on 2016-01-07)
# only numeric / continuous features

# read ".data" file directly with base function read.table()
x <- read.table("classification/spambase/spambase.data", sep = ",")
x$V58 <- as.factor(x$V58)
# 4601 x 58, V58 is the label column
dim(x)

# Split training and testing datasets
in_train <- createDataPartition(x$V58, p = 0.7, list = F)
training <- x[in_train, ]
testing <- x[-in_train, ]


model_nb <- naiveBayes(V58 ~ ., data = training)
results <- predict(model_nb, testing[, -58])
confusionMatrix(results, testing[, 58]) # accuracy: 0.7194
