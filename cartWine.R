#  Group           : Wine Quality
#  HW              : Final Project
#  Algorithm       : CART

rm(list = ls())

library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)

data <- read.csv('winequality-red.csv', header=TRUE)
data <- na.omit(data)
data$quality = as.factor(data$quality)

set.seed(123)

idx <- sort(sample(nrow(data), as.integer((.70 * nrow(data)))))
training <- data[idx, ]
test <- data[-idx, ]

# vec <- c(0,   1,   2,   1,   2,  1,  1,
#          0.9, 0,   2,   1,   1,   1,  1,
#          1,   1,   0,   1.6, 1,   1,  1,
#          0.9, 1.1, 2.0, 0,   1.6, 1,  1,
#          1,   1.1, 1.2, 2.4, 0,   1,  1,
#          1,   1,   1,   1,   1,   0,  1,
#          1,   1,   1,   1,   1,   1,  0)
# parms = list(loss = cost)

# cost = matrix(vec, nrow = 7, byrow = TRUE)

cartClass <- rpart(as.factor(quality)~., data=training)
cartClass = prune(cartClass, cp = 0.01)
cartPredict <- predict(cartClass, test, type='class')
cm <- table(Actual=test$quality, CART=cartPredict)

cartWrong <- sum(test$quality != cartPredict)
cartWrongRate <- cartWrong / length(test$quality)

# prp(cartClass)

# much fancier graph
# fancyRpartPlot(cartClass)

print(paste("Accuracy Rate is: ", (1 - cartWrongRate) * 100))