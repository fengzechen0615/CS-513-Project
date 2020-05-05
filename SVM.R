#  Group           : Wine Quality
#  HW              : Final Project
#  Algorithm       : SVM

library(e1071)

data<- read.csv('/Users/allison/Documents/2020Spring/CS-513/wine/winequality-red.csv')

train_indices = sample(nrow(data),floor(0.7*nrow(data)))
train = data[train_indices,]
test = data[-train_indices,]

temp = train
temp$quality[which(temp$quality != 3)] = 0
temp$quality[which(temp$quality == 3)] = 1
model3 = svm(quality ~ . , data = temp)

temp = train
temp$quality[which(temp$quality != 4)] = 0
temp$quality[which(temp$quality == 4)] = 1
model4 = svm(quality ~ . , data = temp)

temp = train
temp$quality[which(temp$quality != 5)] = 0
temp$quality[which(temp$quality == 5)] = 1
model5 = svm(quality ~ . , data = temp)

temp = train
temp$quality[which(temp$quality != 6)] = 0
temp$quality[which(temp$quality == 6)] = 1
model6 = svm(quality ~ . , data = temp)

temp = train
temp$quality[which(temp$quality != 7)] = 0
temp$quality[which(temp$quality == 7)] = 1
model7 = svm(quality ~ . , data = temp)

temp = train
temp$quality[which(temp$quality != 8)] = 0
temp$quality[which(temp$quality == 8)] = 1
model8 = svm(quality ~ . , data = temp)

accuracy = 0
for(i in 1:nrow(test))
{
  result = c(0,0,0,0,0,0)
  result[1] = predict(model3,test[i,])
  result[2] = predict(model4,test[i,])
  result[3] = predict(model5,test[i,])
  result[4] = predict(model6,test[i,])
  result[5] = predict(model7,test[i,])
  result[6] = predict(model8,test[i,])
  quality = which.max(result) + 2
  accuracy = accuracy + (quality == test$quality[i])
}
accuracy = accuracy/nrow(test)
cat ('Accuracy is ',accuracy)

#Accuracy is  0.6395833



