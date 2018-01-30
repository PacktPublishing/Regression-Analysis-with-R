setwd ("c://R")
SeedData=read.csv ("seeds_dataset.csv", header=TRUE,sep="")
str(SeedData)

names(SeedData) = c('Area', 'Perimeter', 'Compactness', 'LengthK',
  'WidthK','AsymCoef','LengthKG','Seeds')
summary(SeedData)

pairs(SeedData[1:7], cex.axis=0.4,pch=16,cex=0.3,cex.labels = 0.6,tck=-0.1)


R<-cor(SeedData[1:7])
R

PCAObj<-prcomp(SeedData[1:7])
print(PCAObj)


plot(PCAObj, type= "l",cex.axis=0.6,cex.main=0.6,cex.lab=0.6)


SumPCAObj<-summary(PCAObj)

barplot(SumPCAObj$importance[2,])
ScreePlot <- barplot(SumPCAObj$importance[2,]*100,ylim = c(0, 110))
lines(x = ScreePlot, y = SumPCAObj$importance[3,]*100)
points(x = ScreePlot, y = SumPCAObj$importance[3,]*100)


biplot(PCAObj,scale=0, cex=0.6,cex.axis=0.6,cex.lab=0.6)

