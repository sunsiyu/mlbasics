library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease, package="AppliedPredictiveModeling")

set.seed(3433)
adData <- data.frame(diagnosis, predictors)
inTrain <- createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training <- adData[inTrain, ]
testing <- adData[-inTrain, ]

subtrain <- training[, c(1, grep(pattern = "^IL", names(training)))]
subtest <- testing[, c(1, grep(pattern = "^IL", names(testing)))]


subpreProc <- preProcess(subtrain, method = "pca", thresh = 0.8)
subtrainPC <- predict(subpreProc, subtrain)
# train training dataset(after preprocessing) with method glm
modFit2 <- train(subtrain$diagnosis ~ ., method = "glm", data = subtrainPC)
# testing dataset also needs preprocessing with PCA
subtestPC <- predict(subpreProc, subtest)







modFit1 <- train(diagnosis ~ ., method ="glm", data=subtrain)
# ctrl <- trainControl(preProcOptions = list(thresh = 0.8))
# modFit2 <- train(diagnosis ~ ., method ="glm", preProcess="pca", data=training)

cfmat1 <- confusionMatrix(subtest$diagnosis, predict(modFit1, subtest))
cfmat2 <- confusionMatrix(subtest$diagnosis, predict(modFit2, subtestPC))

