library(readxl)

setwd ("c://R")
LicDrivers <- read_excel("LicensedDrivers.xlsx")

str(LicDrivers)

LModel = lm(LicensedDrivers~Year,data = LicDrivers)

plot(LicDrivers$Year,LicDrivers$LicensedDrivers,cex = 1.3,lwd = 10)
abline(LModel,lwd=5)

summary(LModel)

names(LModel)
LModel$coefficients
LModel$residuals
stem (LModel$residuals)
LModel$fitted.values

plot(LModel,which=1)
plot(LModel,which=2)
plot(LModel,which=3)
plot(LModel,which=5)
