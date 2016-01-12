# ============================================================================
# This script with only be about classification.
# 2016-01-07
# Author: Siyu Sun
# ============================================================================

library(caret)
library(e1071)
library(mlbench)

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



## Data Sonar - example intro caret
data(Sonar)
set.seed(107)
inTrain <- createDataPartition(y = Sonar$Class, p = 0.75, list = F)

training <- Sonar[inTrain, ]
testing <- Sonar[-inTrain, ]


# use partial least squares discriminant analysis (PLSDA) model
# to tune over the # PLS components that should be retained

plsFit <- train(Class ~ ., 
                data = training, 
                method = "pls", 
                preProcess = c("center", "scale"))


plsFit2 <- train(Class ~ ., 
                 data = training, 
                 method = "pls", 
                 tuneLength = 15, 
                 preProcess = c("center", "scale"))


# To modify the resampling method (default: bootstrap)
# use trainControl, method: boot, repeatedcv, etc.
ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 3)

plsFit3 <- train(Class ~ ., 
                 data = training, 
                 method = "pls", 
                 tuneLength = 15, 
                 trControl = ctrl, 
                 preProcess = c("center", "scale"))



# Finally, to choose diff. measures of performance, additional argu 
# are given to trainControl
ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 3, 
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)
plsFit4 <- train(Class ~ ., 
                 data = training, 
                 method = "pls", 
                 tuneLength = 15, 
                 trControl = ctrl, 
                 metric = "ROC", 
                 preProcess = c("center", "scale"))

# Visualize the results
plot(plsFit4)

# Predict new data
plsClasses <- predict(plsFit4, newdata = testing)

# Predict new data with probabilites as output
plsProbs <- predict(plsFit4, newdata = testing, type = "prob")


# Compute the confusion matrix and associated statistics for the model fit
confusionMatrix(data = plsClasses, testing$Class)













