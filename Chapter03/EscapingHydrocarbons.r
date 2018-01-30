setwd ("c://R")
PetrolData=read.csv ("EscapingHydrocarbons.csv", header=TRUE,sep=";")
str(PetrolData)

n<-names(PetrolData)
form1 = as.formula(paste("AmountEscapingHydrocarbons ~", paste(n[!n %in% "AmountEscapingHydrocarbons"], collapse = " + ")))

MLModel = lm(form1,data =PetrolData)
MLModel

summary(MLModel)

Pred <- predict(MLModel)

plot(PetrolData[,5],Pred,
     xlab="Actual",ylab="Predicted",cex = 1.3,lwd = 10)
abline(a=0,b=1,lwd = 5)

par(mfrow=c(2,2))
plot(MLModel)
