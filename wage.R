# Wage Prediction
library(ISLR) # dataset package
library(ggplot2)
library(caret)
library(splines)
data(Wage)
summary(Wage)
str(Wage)

inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = F) # index of training set
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]
dim(training)

# Lattice Plotting of Predictor Variables (feature plotting)
featurePlot(x = training[, c("age", "jobclass", "education")], 
            y = training$wage, 
            plot = "pairs")
# making factors
cutWage <- Hmisc::cut2(training$wage, g = 3)
table(cutWage)

# Covariate Creation
# Basic idea - convert factor variables to indicator variables
table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass, data = training)
head(predict(dummies, newdata = training))

# Remove zero covariates (have one unique value, identify near 0 variance predictor)
nsv <- nearZeroVar(training, saveMetrics = TRUE)

# Spline Basis
bsBasis <- splines::bs(training$age, df=3)
# col.1 original par
# col.2 square
# col.3 cube

# Fitting curves with spline
lm1 <- lm(wage ~ bsBasis, data = training)
plot(training$age, training$wage, pch = 19, cex = 0.5)
points(training$age, predict(lm1, newdata = training), col="red", pch = 19, cex = 0.5)

p <- ggplot(training, aes(age, wage, color = education)) + geom_jitter()
p + geom_line(aes(y=predict(lm1, newdata = training)), color="red")

# Fit a linear model
modFit <- train(wage ~ age + jobclass + education, method = "lm", data = training)
finMod <- modFit$finalModel
print(modFit)

# Diagnositic Plot
plot(finMod, 1, pch = 19, cex = 0.5, col = "#00000010") # can see outliers
# find what influence outliers by plotting other variables


# With all covariates
modFitAll <- train(wage ~ ., method = "lm", data = training)
pred <- predict(modFitAll, testing)
qplot(wage, pred, data=testing)


