#  Group           : Wine Quality
#  HW              : Final Project
#  Algorithm       : C50

rm(list = ls())

library('C50')

data <- read.csv('winequality-red.csv', header=TRUE)
data <- na.omit(data)
data$quality = as.factor(data$quality)

set.seed(123)

idx <- sort(sample(nrow(data), as.integer((.70 * nrow(data)))))
training <- data[idx, ]
test <- data[-idx, ]

# fit <- C5.0(as.factor(quality)~., data=training, 
#              control = C5.0Control(winnow = TRUE))
# summary(fit)

# find the best cf value

# err.rate <- function(training, test){
#   alpha <- NULL
#   res <- NULL
#   for (i in seq(0.25, 0.1, -0.01)){
#     fit <- C5.0(as.factor(quality)~., data=training, control = C5.0Control(CF = i))
#     pred <- predict(fit, test)
#     freq <- table(pred, test$quality)
#     accuracy <- sum(diag(freq))/sum(freq)
#     alpha <- c(alpha,i)
#     res <- c(res,accuracy)
#   }
#   return(data.frame(alpha, res))
# }
# err <- err.rate(training, test)

# alpha       res
# 1   0.25 0.5979167
# 2   0.24 0.5979167
# 3   0.23 0.5979167
# 4   0.22 0.5958333
# 5   0.21 0.5833333
# 6   0.20 0.5833333
# 7   0.19 0.5833333
# 8   0.18 0.5854167
# 9   0.17 0.5854167
# 10  0.16 0.5854167
# 11  0.15 0.5854167
# 12  0.14 0.6020833
# 13  0.13 0.6166667
# 14  0.12 0.6166667
# 15  0.11 0.6145833
# 16  0.10 0.6145833

# CF = 0.20

C50Class <- C5.0(as.factor(quality)~., data=training, control=C5.0Control(CF = 0.20), trials=60)

C50Predict <- predict(C50Class, test, type="class")
table(actual=test$quality, C50=C50Predict)

wrong <- (test$quality != C50Predict)
C50Rate <- sum(wrong)/length(test$quality)

print(paste("Accuracy Rate is: ", (1-C50Rate)*100))