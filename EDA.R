#  Group           : Wine Quality
#  HW              : Final Project
#  Algorithm       : EDA

library(corrplot)
library(ggplot2)

redwine<- read.csv('/Users/allison/Documents/2020Spring/CS-513/wine/winequality-red.csv')

#Create a variable indicating if a wine is good or bad
redwine$good.wine<-ifelse(redwine$quality>6,1,0)

#Let's look at some summary statistics
str(redwine)
summary(redwine)

#Scatterplot Matrix of Variables
plot(redwine)

#Correlation Heatmap of Variables
corrplot(cor(redwine))

#Distribution of red wine quality ratings
ggplot(redwine,aes(x=quality))+geom_bar(stat = "count",position = "dodge")+
  scale_x_continuous(breaks = seq(3,8,1))+
  ggtitle("Distribution of Red Wine Quality Ratings")+
  theme_classic()

#Distribution of good/bad red wines
ggplot(redwine,aes(x=good.wine,fill=factor(good.wine)))+geom_bar(stat = "count",position = "dodge")+
  scale_x_continuous(breaks = seq(0,1,1))+
  ggtitle("Distribution of Good/Bad Red Wines")+
  theme_classic()
