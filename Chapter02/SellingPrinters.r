setwd ("c://R")
Printers=read.csv ("SellingPrinters.csv", header=TRUE,sep=";")
head(Printers)
str(Printers)

plot(Printers$Sold.Items,Printers$Price)

cov(Printers$Sold.Items,Printers$Price)
cor(Printers$Sold.Items,Printers$Price)


CovX_Y=cov(Printers$Sold.Items,Printers$Price)
CovX_Y
SdSoldItems=sd(Printers$Sold.Items)
SdSoldItems
SdPrice=sd(Printers$Price)
SdPrice
CorrCoef=CovX_Y/(SdSoldItems*SdPrice)
CorrCoef

LinMod <- lm( Printers$Price ~ Printers$Sold.Items,data=Printers)
LinMod

summary(LinMod)


abline(LinMod)
