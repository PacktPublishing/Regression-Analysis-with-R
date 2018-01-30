setwd ("c://R")
CementData=read.csv ("CementData.csv", header=TRUE,sep=",")
str(CementData)

x <- as.matrix(cbind(rep(1,13), CementData[,1:4]))
y <- CementData[,5]


Beta<-solve(t(x)%*%x)%*%t(x)%*%y
Beta

HeatRegFit<- x%*%Beta[,1]
Residual=y-HeatRegFit

plot(Residual,cex = 1.3,lwd = 10)
abline(a=0,b=0,lwd=5)

Rsq = 1 - sum((y - HeatRegFit)^2)/sum((y - mean(y))^2)
Rsq
