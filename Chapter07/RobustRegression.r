setwd ("c://R")
Printers=read.csv ("SellingPrintersOutliers.csv", header=TRUE,sep=";")
str(Printers)
summary(Printers)

plot(Printers$Sold.Items,Printers$Price)


LinMod1 <- lm(Printers$Price~Printers$Sold.Items,data=Printers)
SumLinMod1<-summary(LinMod1)

library(MASS)

LinMod2 <- rlm(Printers$Price~Printers$Sold.Items,data=Printers, psi = psi.hampel, init = "lts")
SumLinMod2<-summary(LinMod2)

LM1Coef <- coef(LinMod1)
LM2Coef <- coef(LinMod2)

par(mfrow=c(1,2))
plot(Printers$Sold.Items,Printers$Price)
abline(coef=LM1Coef)
plot(Printers$Sold.Items,Printers$Price)
abline(coef=LM2Coef)


plot(LinMod2$w, ylab="Weight")
