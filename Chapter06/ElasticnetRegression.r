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

ElasticnetMod <- glmnet(x, y, alpha=0.5, nlambda=100,
                        lambda.min.ratio=0.0001)
plot(ElasticnetMod)

CvElasticnetMod <- cv.glmnet(x, y, alpha=0.5, nlambda=100,
                             lambda.min.ratio=0.0001)

plot(CvElasticnetMod)


