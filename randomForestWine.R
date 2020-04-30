#  Group           : Wine Quality
#  HW              : Final Project
#  Algorithm       : Random

rm(list = ls())

library("randomForest")

data <- read.csv('WineQuality.csv', header=TRUE)
data <- na.omit(data)
data$type = as.integer(as.factor(data$type))
data$quality = as.integer(as.factor(data$quality))

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

# rate
# plot(rate)

# 0.6631549 0.6101290 0.6119128 0.6112290 0.6072801 0.6086186 0.6064997 0.6072955 0.6089973 0.6084176 0.6109786 0.6101957
# mtry = 7

# find the best ntree
# rf <- randomForest(as.factor(quality)~., data=training, mtry=7, importance=TRUE, ntree=1000)
# plot(rf)

# when trees equal 800, the model is becoming steady.

randomForest <- randomForest(as.factor(quality)~., data=training, mtry=7, importance=TRUE, ntree=500, proximity=TRUE)

# importance(randomForest)
# max alcohol
# min type

# varImpPlot(randomForest)

randomForestPrediction <- predict(randomForest, test)

table(actual=test$quality, Prediction=randomForestPrediction)

wrong <- (test$quality != randomForestPrediction)
randomForestRate = sum(wrong)/length(test$quality)

print(paste("Accuracy Rate is: ", (1-randomForestRate)*100))