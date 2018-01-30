BHData <- read.table(url("https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data"), sep = "")
names(BHData)<- c("crim","zn","indus","chas","nox","rm","age","dis",   
                  "rad","tax","ptratio","black","lstat","medv") 
                  
str(BHData)
summary(BHData)

max_data <- apply(BHData, 2, max)
min_data <- apply(BHData, 2, min)

BHDataScaled <- as.data.frame(scale(BHData,center = min_data, scale = max_data - min_data))
summary(BHDataScaled)


boxplot(BHDataScaled)


CorBHData<-cor(BHDataScaled)

library(corrplot)

corrplot(CorBHData, method = "pie",tl.cex=0.4,cl.cex=0.4,type="lower")


LModel1<-lm(medv~.,data=BHDataScaled)
LModel1

summary(LModel1)

Pred1 <- predict(LModel1)
mse1 <- mean((BHDataScaled$medv - Pred1)^2)
mse1



plot(BHDataScaled[,14],Pred1,
     xlab="Actual",ylab="Predicted")
abline(a=0,b=1)



par(mfrow=c(2,2))
plot(LModel1)



library(randomForest)
RFModel=randomForest(medv ~ . , data = BHDataScaled) 
RFModel
summary(RFModel)

par(mfrow=c(1,1))
plot(RFModel)


VarImp<-importance(RFModel)
VarImp<-as.matrix(VarImp[order(VarImp[,1], decreasing = TRUE),])
VarImp


varImpPlot(RFModel)


Pred2<-predict(RFModel)

plot(BHDataScaled[,14],Pred2,
     xlab="Actual",ylab="Predicted")
abline(a=0,b=1)

