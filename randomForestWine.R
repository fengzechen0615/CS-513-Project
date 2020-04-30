#  Group           : Wine Quality
#  HW              : Final Project
#  Algorithm       : Random

rm(list = ls())

library("randomForest")

# class = 7

# data <- read.csv('winequality-red.csv', header=TRUE)
# data <- na.omit(data)
# data$quality = as.integer(as.factor(data$quality))
# 
# set.seed(123)
# 
# idx <- sort(sample(nrow(data), as.integer((.70 * nrow(data)))))
# training <- data[idx, ]
# test <- data[-idx, ]

# find the best mtry
# n <- length(names(training))
# rate = 1
# for (i in 1: (n - 1)) {
#   print(i)
#   set.seed(100)
#   rf = randomForest(as.factor(quality)~., data=training, ntree=1000, mtry=i)
#   rate[i] <- mean(as.numeric(rf$err.rate))
#   print(rf)
# }
# 
# rate
# plot(rate)

# 0.6232179 0.6222972 0.6197441 0.6202953 0.6223850 0.6186066 0.6191017 0.6230324 0.6199652 0.6203628 0.6206012
# which.min(rate)
# mtry = 6

# find the best ntree
# rf <- randomForest(as.factor(quality)~., data=training, mtry=6, importance=TRUE, ntree=1000)
# plot(rf)

# when trees equal 600, the model is becoming steady.

# randomForest <- randomForest(as.factor(quality)~., data=training, mtry=6, importance=TRUE, ntree=600, proximity=TRUE)

# importance(randomForest)
# max alcohol
# min type

# varImpPlot(randomForest)

# randomForestPrediction <- predict(randomForest, test)
# 
# table(actual=test$quality, Prediction=randomForestPrediction)
# 
# wrong <- (test$quality != randomForestPrediction)
# randomForestRate = sum(wrong)/length(test$quality)
# 
# print(paste("Accuracy Rate is: ", (1-randomForestRate)*100))
# "Accuracy Rate is: 71"


# class = 3

data <- read.csv('winequality-red.csv', header=TRUE)
data <- na.omit(data)
# data$quality = as.factor(data$quality)

data$quality[which(data$quality %in% c(3, 4, 5))] = "low"
data$quality[which(data$quality %in% c(6))] = "medium"
data$quality[which(data$quality %in% c(7, 8, 9))] = "high"

set.seed(123)

idx <- sort(sample(nrow(data), as.integer((.70 * nrow(data)))))
training <- data[idx, ]
test <- data[-idx, ]

# find the best mtry
# n <- length(names(training))
# rate = 1
# for (i in 1: (n - 1)) {
#   print(i)
#   set.seed(100)
#   rf = randomForest(as.factor(quality)~., data=training, ntree=1000, mtry=i)
#   rate[i] <- mean(as.numeric(rf$err.rate))
#   print(rf)
# }
# 
# rate
# plot(rate)

# 0.3354853 0.3297664 0.3281737 0.3200239 0.3245834 0.3239456 0.3223861 0.3262303 0.3239999 0.3273806 0.3255801
# which.min(rate)
# mtry = 4

# find the best ntree
# rf <- randomForest(as.factor(quality)~., data=training, mtry=4, importance=TRUE, ntree=1000)
# plot(rf)

# when trees equal 600, the model is becoming steady.

randomForest <- randomForest(as.factor(quality)~., data=training, mtry=4, importance=TRUE, ntree=600, proximity=TRUE)
randomForestPrediction <- predict(randomForest, test)

table(actual=test$quality, Prediction=randomForestPrediction)

wrong <- (test$quality != randomForestPrediction)
randomForestRate = sum(wrong)/length(test$quality)

print(paste("Accuracy Rate is: ", (1-randomForestRate)*100))
# "Accuracy Rate is: 73"