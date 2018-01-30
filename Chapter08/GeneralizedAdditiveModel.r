data(stackloss)
str(stackloss)
summary(stackloss)

pairs(stackloss, panel = panel.smooth)

LModel <- lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., data=stackloss)
summary(LModel)

par(mfcol = c (2,2))
plot(LModel)


library(mgcv) 

GAMModel <- gam(stack.loss ~ s(Air.Flow, k=7) +
                  + s(Water.Temp, k=7) + s(Acid.Conc., k=7), data=stackloss)
summary(GAMModel)

par(mfcol = c (1,1))
plot(GAMModel,select=2)


anova(LModel,GAMModel)
