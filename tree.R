# ================================================
# DECISION TREE EXAMPLES
# Date: 2015-11-22
# Author: Siyu Sun
# Reference: Practical Machine Learning (coursera)
# ================================================

library(ggplot2)
library(caret)
data(iris)  # 150 obs x 5 var, 3 species to be predicted
table(iris$Species)

inTrain <- createDataPartition(iris$Species, p=0.7, list=F)
training <- iris[inTrain, ]
testing <- iris[-inTrain, ]
modFit <- train(Species ~ ., method = "rpart", data = training)
print(modFit$finalModel)  # to see the nodes and splits
prediction <- predict(modFit, newdata = testing)

# Plot of Classification Tree
plot(modFit$finalModel, uniform = T, main = "Classification Tree")
text(modFit$finalModel, use.n = T, all = T, cex = 0.8)
# library(rattle)
# fancyRpartPlot(modFit$finalModel)
