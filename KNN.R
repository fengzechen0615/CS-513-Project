#  Group           : Wine Quality
#  HW              : Final Project
#  Algorithm       : KNN

library(caret)
library(tidyverse)

wine<- read.csv('/Users/allison/Documents/2020Spring/CS-513/wine/winequality-red.csv')
wine <- wine %>% 
  mutate(good = ifelse(quality >= 6, 1, 0))

inTraining <- createDataPartition(wine$good, p = .7, list = FALSE)
training <- wine[ inTraining ,]
testing <- wine[ - inTraining, ]

ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

carettrain <- training %>%
  mutate(good = factor(good)) %>%
  select(-quality)

carettest <- testing %>%
  mutate(good = factor(good)) %>%
  select(-quality)

knnfit <- train(good ~ ., data = carettrain, method = "knn", trControl = ctrl)
knnpred <- predict(knnfit, newdata = carettest)
confusionMatrix(data = knnpred, carettest$good)


#Accuracy : 0.6263



