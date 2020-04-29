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

randomForest <- randomForest(quality~., data=training[, -1], importance=TRUE, ntree=500, na.action=na.roughfix)

importance(randomForest)
# max alcohol 
# min type

varImpPlot(randomForest)
plot(randomForest)
MDSplot(randomForest, data$quality)

randomForestPrediction <- round(predict(randomForest, test), 0)

table(actual=test$quality, Prediction=randomForestPrediction)

wrong <- (test$quality != randomForestPrediction)
randomForestRate = sum(wrong)/length(test$quality)

print(paste("Accuracy Rate is: ", (1-randomForestRate)*100))