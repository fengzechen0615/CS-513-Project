#  Group           : Wine Quality
#  HW              : Final Project
#  Algorithm       : NB


library(e1071)
library(caret)

data<- read.csv('/Users/allison/Documents/2020Spring/CS-513/wine/winequality-red.csv')

smp_siz <- floor(0.7*nrow(data)) 
set.seed(42)   

standardize <- function(x)
  {
    x/max(x)
  } 

last_col <- ncol(data) 

data$quality <- as.factor(data$quality)
data[,-last_col] <- as.data.frame(lapply(data[,-last_col], standardize))

train_ind <- sample(seq_len(nrow(data)),size = smp_siz) 
train <- data[train_ind,]
test<- data[-train_ind,] 
NB_M <- naiveBayes(quality ~., data=train, usekernel=FALSE)

NB_Pred <- predict(NB_M,newdata=test)
confusionMatrix(data = as.factor(NB_Pred),reference = test$quality)

#Accuracy : 0.55


