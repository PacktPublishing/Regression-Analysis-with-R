PD<-rpois(60, lambda = 5)
PD
FPD<-table(PD)

plot(FPD,cex.axis=0.6,xlab="Counts",ylab="Frequency of counts")



WPData<-as.data.frame(warpbreaks)
str(WPData)
summary(WPData)

par(mfrow = c(1, 2))
plot(breaks ~ tension, data = WPData, col = "lightgray",
     varwidth = TRUE, subset = wool == "A", main = "Wool A",ylim=c(8,72))
plot(breaks ~ tension, data = warpbreaks, col = "lightgray",
     varwidth = TRUE, subset = wool == "B", main = "Wool B",ylim=c(8,72))


WPModel1 <- glm(breaks ~ wool+tension, data = WPData, family=poisson)
WPModel1

CoefWP1<-coef(WPModel1)

CoefWP1[2]
exp(CoefWP1[2])

summary(WPModel1)

deviance(WPModel1)/df.residual(WPModel1)

WPModel2<-glm(breaks~wool*tension, data = WPData, family=poisson)
WPModel2
summary(WPModel2)

deviance(WPModel2)/df.residual(WPModel2)


