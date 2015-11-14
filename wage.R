# Wage Prediction
library(ISLR) # dataset package
library(ggplot2)
library(caret)
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
