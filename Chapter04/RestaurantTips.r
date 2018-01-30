setwd("C:/R")
data=read.csv('RestaurantTips.csv',sep=",",header=TRUE)
View(data)
names(data)

plot(data[,2:5],cex.axis=0.6,cex.lab=0.6,cex.labels=0.6,cex=0.6,pch=16)

cor(data[,2:5])

formula1=CustomerWillTip ~ Service + Food + Ambience
LGModel<-glm(formula1 , data = data, family = binomial(logit))
summary(LGModel)

LGModel1<-glm(CustomerWillTip ~ Service, data = data, family = binomial(logit))
summary(LGModel1)

LGModel1Pred <- round(predict(LGModel1, data, type="response"))

library(caret)
LGModel1CM <- confusionMatrix(LGModel1Pred, data[,"CustomerWillTip"])
LGModel1CM
