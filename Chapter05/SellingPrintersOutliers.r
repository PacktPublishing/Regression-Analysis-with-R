setwd ("c://R")
Printers1=read.csv ("SellingPrinters.csv", header=TRUE,sep=";")
str(Printers1)
summary(Printers1)

LinMod1 <- lm( Printers1$Price ~ Printers1$Sold.Items,data=Printers1)
LinMod1

PrintersOutlier<-data.frame(X=c("Store21","Store22"),Sold.Items=c(210,50),Price=c(47,50))
#PrintersOutlier<-data.frame(X=c("Store21","Store22","Store23","Store24","Store25"),Sold.Items=c(180,200,210,50,90),Price=c(47,46,45,49,50))
Printers2 <- rbind(Printers1,PrintersOutlier)

LinMod2 <- lm( Printers2$Price ~ Printers2$Sold.Items,data=Printers2)
LinMod2


par(mfrow=c(1, 2))
plot(Printers1$Sold.Items,Printers1$Price,xlim=c(48, 212), ylim=c(37, 62),cex.axis=0.6,cex.lab=0.6)
abline(LinMod1)
plot(Printers2$Sold.Items,Printers2$Price,xlim=c(48, 212), ylim=c(37, 62),cex.axis=0.6,cex.lab=0.6)
abline(LinMod2)
