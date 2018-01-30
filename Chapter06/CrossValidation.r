library(caret)
data(mtcars)
str(mtcars)
summary(mtcars)
R<-cor(mtcars)


DataSplit<-createDataPartition(y = mtcars$mpg, p = 0.7, list = FALSE)
TrainData<-mtcars[DataSplit,]
TestData<-mtcars[-DataSplit,]
LmFit1<-train(mpg~., data = TrainData, method = "lm")
summary(LmFit1)

PredictedTest<-predict(LmFit1,TestData)
ModelTest1<-data.frame(obs = TestData$mpg, pred=PredictedTest)
defaultSummary(ModelTest1)

Control1<-trainControl(method = "cv",number = 10)
LmFit2<-train(mpg ~ ., data = mtcars, method = "lm", trControl = Control1, metric="Rsquared")
summary(LmFit2)

PredictedTest2<-predict(LmFit2,mtcars)
ModelTest2<-data.frame(obs = mtcars$mpg, pred=PredictedTest2)
defaultSummary(ModelTest2)

residuals<-residuals(LmFit2)
PredictedValues2<-predict(LmFit2)

par(mfrow=c(2,1))
plot(mtcars$mpg,residuals,cex.axis=0.6,cex.lab=0.6)
abline(0,0)
plot(mtcars$mpg,PredictedValues2,xlim=c(10,35),ylim=c(10,35),cex.axis=0.6,cex.lab=0.6)
abline(0,1)


Control2<-trainControl(method="LOOCV")
LmFit3<-train(mpg ~ ., data = mtcars, method = "lm", trControl = Control2)
summary(LmFit3)

varImp(LmFit3)


plot(varImp(LmFit3),cex.axis=0.2,cex.lab=0.1)


Control3<-trainControl(method="boot", number=100)
LmFit4<-train(mpg ~ ., data = mtcars, method = "lm", trControl = Control3)
summary(LmFit4)

PredictedTest4<-predict(LmFit4,mtcars)
ModelTest4<-data.frame(obs = mtcars$mpg, pred=PredictedTest4)
defaultSummary(ModelTest4)
