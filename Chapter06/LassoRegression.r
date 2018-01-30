Seatbelts <- data.frame(Seatbelts)
str(Seatbelts)
head(Seatbelts,n=10)
summary(Seatbelts)

BeforeLaw <-subset(Seatbelts,law ==0)
AfterLaw <- subset(Seatbelts,law!=0)

summary(BeforeLaw)
summary(AfterLaw)


library(glmnet)

x <- model.matrix(DriversKilled~., BeforeLaw)[,-c(1,8)]
y <- BeforeLaw$DriversKilled

LassoMod <- glmnet(x, y, alpha=1, nlambda=100,   
                   lambda.min.ratio=0.0001)

plot(LassoMod,xvar="norm",label=TRUE)


CvLassoMod <- cv.glmnet(x, y, alpha=1, nlambda=100,
                        lambda.min.ratio=0.0001)

plot(CvLassoMod)

best.lambda <- CvLassoMod$lambda.min
best.lambda

coef(CvLassoMod, s = "lambda.min")
