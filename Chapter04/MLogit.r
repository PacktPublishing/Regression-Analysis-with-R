library(mlogit)

data("Heating")
str(Heating)

DataHeating <- mlogit.data(Heating, shape="wide", choice="depvar", varying=c(3:12))
head(DataHeating, 10)

form1 <- mFormula(depvar~ic+oc|0)
MlogitModel <- mlogit(form1, DataHeating)
summary(MlogitModel)
