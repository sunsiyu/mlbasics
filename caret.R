library(caret)
library(kernlab)
library(AppliedPredictiveModeling)

#--------------------------- VISUALIZATION ------------------------------------#
## Scatterplot Matrix
transparentTheme(trans = 0.4)
featurePlot(x = iris[, 1:4],
            y = iris$Species, 
            plot = "pairs", 
            ## Add a key at the top
            auto.key = list(columns = 3))

## Scatterplot Matrix with Ellipses
featurePlot(x = iris[, 1:4],
            y = iris$Species,
            plot = "ellipse",
            ## Add a key at the top
            auto.key = list(columns = 3))

## Overlayed Density Plot
transparentTheme(trans = 0.9)
featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "density", 
            ## Pass in options to xyplot() to make it prettier
            scales = list(x = list(relation = "free"),
                          y = list(relation = "free")),
            adjust = 1.5,
            pch = "|", 
            layout = c(4, 1), 
            auto.key = list(column = 3))

## Box Plots
featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "box", 
            # Pass in options to bwplot()
            scales = list(x = list(relation = "free"),
                          y = list(rot = 90)),
            layout = c(4, 1),
            auto.key = list(column = 2))


## Scatter Plots
library(mlbench)
data(BostonHousing)
regVar <- c("age", "lstat", "tax")
str(BostonHousing[, regVar])
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
featurePlot(x = BostonHousing[, regVar],
            y = BostonHousing$medv,
            plot = "scatter",
            layout = c(3, 1))
# add a smoother
featurePlot(x = BostonHousing[, regVar],
            y = BostonHousing$medv,
            plot = "scatter",
            type = c("p", "smooth"),
            span = .5,
            layout = c(3, 1))

#--------------------------- PRE-PROCESSING -----------------------------------#
















