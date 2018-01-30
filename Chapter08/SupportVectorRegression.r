library(e1071)

x <- seq(0, 7, by = 0.01)
y <- sin(x) + rnorm(x, sd = 0.1)

LModel<-lm(y~x)
summary(LModel)

plot(x, y)
abline(LModel)


PredLModel<- predict(LModel)

mse1 <- mean((y - PredLModel)^2)
mse1


SVModel <- svm(x, y)
summary(SVModel)

PredSVModel <- predict(SVModel)

mse2 <- mean((y - PredSVModel)^2)
mse2


plot(x, y)
points(x, PredSVModel)
abline(LModel)

