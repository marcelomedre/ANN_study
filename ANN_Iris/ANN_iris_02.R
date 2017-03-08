# Working with ANN using nnet
setwd("C:/Users/Marcelo/Desktop/Data Science/ANN_study/ANN_Iris/")

library(parallelSVM)

rm(list = ls())
data <- iris
summary(iris)

# There are four features - Sepal. Length, Sepal. Width, Petal. Length and Petal.Width
# Three categories of Species - Setosa, Versicolor, Virginica 

x <- subset(data, select = -Species)
y <- data$Species

# Create a Model

model <- parallelSVM(x, y)

# Get preditions

predictions <- predict(model, x)

# check quality
table(predictions, y)


## Example 2

library(e1071)

# load train and test data
data(magicData)

# Calculate Model

system.time(serialSVM <- svm(V11 ~., trainData[,-1],
                             probability = TRUE, cost = 10, gamma = 0.1))
system.time(parallelSVM <- parallelSVM(V11 ~., data = trainData[,-1],
                        numCores = 8, samplingSize = 0.2, 
                        probability = TRUE, gamma = 0.1, cost = 10))

# Calculate predictions

system.time(serialPredictions <- predict(serialSVM, testData))

system.time(parallelPredicitions <- predict(parallelSVM, testData))

# Check for quality
table(serialPredictions,testData[,"V11"])
table(parallelPredicitions,testData[,"V11"])
