#  Group           : Wine Quality
#  HW              : Final Project
#  Algorithm       : K-means





## remove all objects
rm(list=ls())
dev.off
#read CSV
setwd("/Users/louyilin/RStudioProjects/FinalProject")
#kdata<-read.csv("winequalityN.csv",header = TRUE,na.strings = "?")
kdata<-read.csv("winequality-red.csv",header = TRUE,na.strings = "?")
standardize <- function(x){x/max(x)} 

#  clean NA datas
kdata=na.omit(kdata)
#print(head(kdata))


#kdata$type <- as.numeric(kdata$type, levels = c("white", "red"), labels = c("1","2"))
kdata[,-12] <- as.data.frame(lapply(kdata[,-12], standardize))
kMean=kmeans(kdata[,-c(12)],3)
plot(kdata[c(11,12)],col=kMean$cluster)
points(kMean$centers,col=1:3,pch=16,cex=2)

kMeanTable=table(clusters=kMean$cluster,quality=kdata[,'quality'])
print(kMeanTable)


fviz_cluster(kMean,data=kdata)




