library(MASS)
library(BAS)
UScrimeData<-as.data.frame(UScrime)
str(UScrimeData)
summary(UScrimeData)

boxplot(UScrimeData)

UScrimeData[,-2] = log(UScrime[,-2])
boxplot(UScrimeData)


USCrimeBas<- bas.lm(y ~ ., data = UScrimeData,prior = 'BIC', modelprior = uniform())
USCrimeBas

summary(USCrimeBas)

image(USCrimeBas, subset=-1)

par(mar=c(2,2,1,1))
par(mgp=c(1,0.5, 0))
par(mfrow=c(2,2))
plot(USCrimeBas)

