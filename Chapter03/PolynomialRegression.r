Time=c(6,7,8,9,10,11,12,13,14,15,16,17,18,19)
Temp=c(4,6,7,9,10,11,11.5,12,12,11.5,11,10,9,8)

plot(Time,Temp,cex.axis=0.6,cex.lab=0.6)


Polyfit2 <- lm(Temp ~ Time + I(Time^2))
Polyfit3 <- lm(Temp ~ Time + I(Time^2) + I(Time^3))

Polyfit2b <- lm(Temp ~ poly(Time, 2, raw=TRUE))
Polyfit3b <- lm(Temp ~ poly(Time, 3, raw=TRUE))

summary(Polyfit2)
summary(Polyfit3)

PredData = data.frame(Time = seq(min(Time), max(Time), length.out = 100))
PredData$Temp = predict(Polyfit2, newdata = PredData)

plot(Time,Temp,cex.axis=0.6,cex.lab=0.6)
with(PredData, lines(x = Time, y = Temp))



PredData2 = data.frame(Time = seq(min(Time), max(Time), length.out = 100))
PredData2$Temp = predict(Polyfit3, newdata = PredData)

plot(Time,Temp,cex.axis=0.6,cex.lab=0.6)
with(PredData2, lines(x = Time, y = Temp))
