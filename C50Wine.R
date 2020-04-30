#  Group           : Wine Quality
#  HW              : Final Project
#  Algorithm       : C50

rm(list = ls())

library('C50')

data <- read.csv('WineQuality.csv', header=TRUE)
data <- na.omit(data)
data$type = as.integer(as.factor(data$type))
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
# 1   0.25 0.5745229
# 2   0.24 0.5740072
# 3   0.23 0.5740072
# 4   0.22 0.5740072
# 5   0.21 0.5724600
# 6   0.20 0.5760701
# 7   0.19 0.5750387
# 8   0.18 0.5755544
# 9   0.17 0.5786488
# 10  0.16 0.5781331
# 11  0.15 0.5791645
# 12  0.14 0.5812274
# 13  0.13 0.5853533
# 14  0.12 0.5760701
# 15  0.11 0.5760701
# 16  0.10 0.5843218

# CF = 0.13

C50Class <- C5.0(as.factor(quality)~., data=training, control=C5.0Control(CF = 0.13), trials=30)

# plot(C50Class)

C50Predict <- predict(C50Class, test, type="class")
table(actual=test$quality, C50=C50Predict)

wrong <- (test$quality != C50Predict)
C50Rate <- sum(wrong)/length(test$quality)

print(paste("Accuracy Rate is: ", (1-C50Rate)*100))