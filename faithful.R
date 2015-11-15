# Linear Regression Simple Example with faithful dataset
library(caret)
data("faithful")
set.seed(333)

inTrain <- createDataPartition(y = faithful$waiting, p = 0.5, list = F)
trainFaith <- faithful[inTrain, ]
testFaith <- faithful[-inTrain, ]

head(trainFaith)
p1 <- ggplot(trainFaith, aes(waiting, eruptions)) + geom_point()
p2 <- ggplot(testFaith, aes(waiting, eruptions)) + geom_point()
lm1 <- lm(eruptions ~ waiting, data = trainFaith)
summary(lm1)

ptrain <- p1 + geom_line(aes(y = lm1$fitted.values), size = 1, color = "red")

# coef(lm1)[1] + coef(lm1)[2] * 80
newdata <- data.frame(waiting = 80)
predict(lm1, newdata)

# Plot Predictions - training and testing
par(mfrow = c(1,2))
ptrain
ptest <- p2 + geom_line(aes(y = predict(lm1, newdata=testFaith)), color="green", size=1)
ptest

# Get Train/Test Errors
# RMSE train
sqrt(sum((lm1$fitted.values - trainFaith$eruptions)^2))
# RMSE test
sqrt(sum((predict(lm1, newdata = testFaith) - testFaith$eruptions)^2))

# Prediction Intervals
pred1 <- predict(lm1, newdata = testFaith, interval = "prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting, testFaith$eruptions)
matlines(testFaith$waiting[ord], pred1[ord, ], type="l")

modFit <- train(eruptions ~ waiting, data = trainFaith, method = "lm")
summary(modFit$finalModel)



