#  Group           : Wine Quality
#  HW              : Final Project
#  Algorithm       : K-means





## remove all objects
rm(list=ls())
dev.off
#read CSV
#setwd("/Users/louyilin/RStudioProjects/FinalProject")
#knnData<-read.csv("winequalityN.csv",header = TRUE,na.strings = "?")
knnData<-read.csv("winequality-red.csv",header = TRUE,na.strings = "?")
standardize <- function(x){x/max(x)} 
#  clean NA datas
knnData=na.omit(knnData)
#knnData$type <- as.numeric(knnData$type, levels = c("white", "red"), labels = c("1","2"))
knnData[,-12] <- as.data.frame(lapply(knnData[,-12], standardize))
kMean=kmeans(knnData[,-c(12)],3)
plot(knnData[c(3,8)],col=kMean$cluster)
points(kMean$centers,col=1:3,pch=16,cex=2)

kMeanTable=table(clusters=kMean$cluster,quality=knnData[,'quality'])
print(kMeanTable)







