data(trees)
str(trees)
summary(trees)

pairs(trees, panel = panel.smooth)


plot(Volume ~ Girth, data = trees, log = "xy",xlab="LogGirth" ,ylab="LogVolume")

coplot(log(Volume) ~ log(Girth) | Height, data = trees,
                                  panel = panel.smooth)

library(earth)

MARSModel <- earth(Volume ~ ., data = trees)

summary(MARSModel)

EvimpMD<-evimp(MARSModel)
EvimpMD

plot(MARSModel,legend.pos=NA)


PredMARSModel <- predict(MARSModel, trees)
# summarize accuracy
mse1 <- mean((trees$Volume - PredMARSModel)^2)
print(mse1)

LMModel <- lm(Volume ~ ., data = trees)
PredLMModel <- predict(LMModel, trees)
mse2 <- mean((trees$Volume - PredLMModel)^2)
print(mse2)