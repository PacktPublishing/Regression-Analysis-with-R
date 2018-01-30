AutoData <- read.table(url("https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data"), sep = "")
names(AutoData)<- c("mpg","cylinders","displacement","horsepower","weight","acceleration",   
                  "year","origin","name") 
str(AutoData)

AutoData<-AutoData[!(AutoData$horsepower=="?"),]
AutoData$horsepower<-as.integer(AutoData$horsepower)
str(AutoData)


plot(AutoData$weight, AutoData$mpg, pch=AutoData$origin)



par(mfrow=c(2,2))
plot(AutoData$cylinders, AutoData$mpg, pch=AutoData$origin)
plot(AutoData$displacement, AutoData$mpg, pch=AutoData$origin)
plot(AutoData$horsepower, AutoData$mpg, pch=AutoData$origin)
plot(AutoData$acceleration, AutoData$mpg, pch=AutoData$origin)
dev.off()

library(neuralnet)

mean_data <- apply(AutoData[1:6], 2, mean)
sd_data <- apply(AutoData[1:6], 2, sd)

AutoDataScaled <- as.data.frame(scale(AutoData[,1:6],center = 
                                        mean_data, scale = sd_data))
head(AutoDataScaled, n=20)


index = sample(1:nrow(AutoData),round(0.70*nrow(AutoData)))
train_data <- as.data.frame(AutoDataScaled[index,])
test_data <- as.data.frame(AutoDataScaled[-index,])

n = names(AutoDataScaled)
f = as.formula(paste("mpg ~", paste(n[!n %in% "mpg"], 
                                    collapse = " + ")))

NNRModel<-neuralnet(f,data=train_data,hidden=3,linear.output=TRUE)
summary(NNRModel)

plot(NNRModel,cex=0.6,cex.axis=0.6,cex.lab=0.6)

NNRModel$result.matrix

PredNetTest <- compute(NNRModel,test_data[,2:6])

MSE.net <- sum((test_data$mpg - PredNetTest$net.result)^2)/nrow(test_data)

LModel <- lm(mpg~., data=train_data)
summary(LModel)

PredLModel <- predict(LModel,test_data)

MSE.lm <- sum((PredLModel - test_data$mpg)^2)/nrow(test_data)


par(mfrow=c(1,2))
plot(test_data$mpg,PredNetTest$net.result,col='black',main="Real vs predicted for neural network")
abline(0,1,lwd=1)
plot(test_data$mpg,PredLModel,col='black',main="Real vs predicted for linear regression")
abline(0,1,lwd=1)

