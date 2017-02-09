# Working with ANN using nnet
setwd("C:/Users/Marcelo/Desktop/Data/ANN_Iris")
library(nnet)
rm(list = ls())
data <- iris
summary(iris)

# There are four features - Sepal. Length, Sepal. Width, Petal. Length and Petal.Width
# Three categories of Species - Setosa, Versicolor, Virginica 

str(iris)
# All numeric variables and a Factor output

library(caTools)
set.seed(101)
# Split any column
split = sample.split(data$Sepal.Length, SplitRatio = 0.7)
# Separating train and test datasets
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)

# Training the model with 2 neurons
iris.net_2 <- nnet(Species ~ . , data = train, size = 2, rang = 0.1, maxit = 250)
net.pred_2 <- predict(iris.net_2, test[,1:4], type = "class") 
table(test$Species, net.pred)

#accuracy
acc_2 = mean((test$Species == net.pred_3))

# Training the model with 6 neurons
iris.net_3 <- nnet(Species ~.,data = train, size = 6, rang = 0.1,
                   maxit = 2500, decay = 1e-2)
net.pred_3 <- predict(iris.net_3, test[,1:4], type = "class") 
net.pred_3
table(test$Species, net.pred_3)

#accuracy
acc_3 = mean((test$Species == net.pred_3))

# Training the model with 8 neurons
iris.net_4 <- nnet(Species ~.,data = train, size = 8, rang = 0.1,
                   maxit = 2500, decay = 1e-2)
net.pred_4 <- predict(iris.net_4, test[,1:4], type = "class") 
net.pred_4
table(test$Species, net.pred_4)

#accuracy
acc_4 = mean((test$Species == net.pred_4))

# NN with 8 hidden neurons with same accuracy than 6 hidden neurons

