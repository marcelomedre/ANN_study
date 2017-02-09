# Programming first NN in R
library(ISLR)

# Loading data from ISLR package
data <- College
# Looking to the data
head(data)
class(data)
str(data) # 777 obs and 18 variables (factor and numeric)

# Normalization is required to train and test a NN, otherwise the
# algorithm will not converge

# Normalization using built_in scale() 
maxs <- apply(College[,2:18], 2, max) # only the numeric columns
mins <- apply(College[,2:18], 2, min)

# Using scale() and convert the result matrix to data frame

scaled.data <- as.data.frame(scale(College[,2:18], center = mins, scale = maxs - mins))
head(scaled.data)

# scale () calculates the mean and standard deviation of the entire vector,
# then "scale" each element by those values by subtracting the mean and 
# dividing by the sd.

# Working on Column 1 "Private"

Private = as.numeric(data$Private)-1
data_scaled = cbind(Private, scaled.data)

library(caTools)
set.seed(101)
# Split any column
split = sample.split(data_scaled$Private, SplitRatio = 0.7)

# Separating train and test datasets
train <- subset(data_scaled, split == TRUE)
test <- subset(data_scaled, split == FALSE)

# The neuralnetwork() function won't accept the typical decimal R format
# for a formula involving all features (e.g. y ~.)

# simple script to create the expanded formula and save us some typing:

feats <- names(scaled.data)
# concatenated strings
f <- paste(feats, collapse = " + ")
f <- paste("Private ~", f)
# 2 steps above is the same of writing Private ~ Apps + Accept + Enroll + ...

f <- as.formula(f)
f

library(neuralnet)
nn <- neuralnet(f, train, hidden = c(10, 10), linear.output = FALSE)

plot(nn)

# Compute Predictions off Test Set
predict.nn.values <- compute(nn, test[2:18])

print(head(predict.nn.values$net.result))

# Creating a confusion matrix
predict.nn.values$net.result <- sapply(predict.nn.values$net.result, round,digits = 0)
table(test$Private, predict.nn.values$net.result)

results <- cbind(test$Private, results$pred)
results <- as.data.frame(results)

colnames(results) <- c("Private", "pred")

library(ggplot2)
png('ANN_test_vs_results.png')
ggplot(results, aes(x = 1:233, y = Private))+
        geom_point()+
        geom_point(aes(y = pred), col = "red")

dev.off()
