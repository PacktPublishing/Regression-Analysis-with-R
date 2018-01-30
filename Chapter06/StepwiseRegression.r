YachtHydrodynamics <- read.table(url("https://archive.ics.uci.edu/ml/machine-learning-databases/00243/yacht_hydrodynamics.data"))
names(YachtHydrodynamics)<- c('LongPos', 'PrismaticCoef', 'LengDispRatio',
  'BeamDraughtRatio', 'LengthBeamRatio','FroudeNumber','ResResistance')

summary(YachtHydrodynamics)

par(mfrow=c(2,3))
plot(YachtHydrodynamics$LongPos,YachtHydrodynamics$ResResistance,xlab="LongPos",ylab="ResResistance")
plot(YachtHydrodynamics$PrismaticCoef,YachtHydrodynamics$ResResistance,xlab="PrismaticCoef",ylab="ResResistance")
plot(YachtHydrodynamics$LengDispRatio,YachtHydrodynamics$ResResistance,xlab="LengDispRatio",ylab="ResResistance")
plot(YachtHydrodynamics$BeamDraughtRatio,YachtHydrodynamics$ResResistance,xlab="BeamDraughtRatio",ylab="ResResistance")
plot(YachtHydrodynamics$LengthBeamRatio,YachtHydrodynamics$ResResistance,xlab="LengthBeamRatio",ylab="ResResistance")
plot(YachtHydrodynamics$FroudeNumber,YachtHydrodynamics$ResResistance,xlab="FroudeNumber",ylab="ResResistance")

Lm1 <- lm(ResResistance ~ ., data = YachtHydrodynamics)
summary(Lm1)
Slm1 <- step(Lm1)

