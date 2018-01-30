Seatbelts <- data.frame(Seatbelts)
str(Seatbelts)
head(Seatbelts,n=10)
summary(Seatbelts)

BeforeLaw <-subset(Seatbelts,law ==0)
AfterLaw <- subset(Seatbelts,law!=0)

summary(BeforeLaw)
summary(AfterLaw)

par(mfrow=c(1,2))
boxplot(BeforeLaw$DriversKilled,ylim=c(50,200),main="Before Law",ylab="Drivers Killed")
boxplot(AfterLaw$DriversKilled,ylim=c(50,200),main="After Law",ylab="Drivers Killed")

library(glmnet)

x <- model.matrix(DriversKilled~., BeforeLaw)[,-c(1,8)]
y <- BeforeLaw$DriversKilled

RidgeMod <- glmnet(x, y, alpha=0, nlambda=100, lambda.min.ratio=0.0001)

plot(RidgeMod,xvar="lambda",label=TRUE)


CvRidgeMod <- cv.glmnet(x, y, alpha=0, nlambda=100, lambda.min.ratio=0.0001)

plot(CvRidgeMod)


best.lambda <- CvRidgeMod$lambda.min
best.lambda

predict(RidgeMod, s=best.lambda, type="coefficients")[1:6, ]