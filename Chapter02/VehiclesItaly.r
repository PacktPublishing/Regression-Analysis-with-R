library(readxl)
library(pracma)

setwd ("c://R")
VehicleData <- read_excel("VehiclesItaly.xlsx")
head(VehicleData)
summary(VehicleData)

y<-VehicleData$Registrations
x<-(VehicleData$Population)
alpha <- mldivide(x,y)

plot(x,y,cex = 1.3,lwd = 10)
abline(a=0,b=alpha,lwd=5)

VehicleRegFit<-c(alpha)*x
Residual=y-VehicleRegFit

plot(Residual,cex = 1.3,lwd = 10)
abline(a=0,b=0,lwd=5)
stem(Residual)

Rsq1 = 1 - sum((y - VehicleRegFit)^2)/sum((y - mean(y))^2)


x1<-cbind(x,1)
alpha_beta <- mldivide(x1,y)
VehicleRegFit2<-x1%*%alpha_beta

plot(x,y,cex = 1.3,lwd = 10)
abline(a=0,b=alpha,lwd=5,col="red")
abline(a=alpha_beta[2,],b=alpha_beta[1,],lwd=5,lty=3)

Rsq2 = 1 - sum((y - VehicleRegFit2)^2)/sum((y - mean(y))^2)
