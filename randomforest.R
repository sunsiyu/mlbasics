library(randomForest)

modelFit <- randomForest(class ~ ., 
                         data=train, 
                         ntree=500, 
                         mtry=5, 
                         importance=TRUE)
model_pred <- predict(modelFit, test)

