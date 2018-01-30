SwissData <- data.frame(swiss)
str(SwissData)
summary(SwissData)

boxplot(SwissData)

R<-cor(SwissData)


plot(SwissData[,1:6])

library(leaps)
SubsetSelection <- regsubsets(Infant.Mortality~., SwissData, nvmax=5)
SumSubsetSelection<-summary(SubsetSelection)
BestSubsetAdjr2<- which.max(SumSubsetSelection$adjr2)

BestSubsetCp<- which.min(SumSubsetSelection$cp)

par(mfrow=c(1,2))
plot(SumSubsetSelection$adjr2, xlab="Variables selected", ylab="Adjusted RSq", type="l")
points(BestSubsetAdjr2, SumSubsetSelection$adjr2[BestSubsetAdjr2], col="black", cex =1, pch =20)
plot(SumSubsetSelection$cp, xlab="Variables selected", ylab="CP", type="l")
points(BestSubsetCp, SumSubsetSelection$cp[BestSubsetCp], col="black", cex =1, pch =20)


coef(SubsetSelection,2)

