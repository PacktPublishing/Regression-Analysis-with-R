diabetes <- matrix(c(326,160,8,64), nrow=2) 
colnames(diabetes) <- c("DiabNO", "DiabYes") 
rownames(diabetes) <- c("StressNO", "StressYes")  
TableDiabetes <- as.table(diabetes)

DfBiabetes <- as.data.frame(TableDiabetes) 

LogModel <- glm(Var2 ~ Var1, weights = Freq, data = DfBiabetes, family = binomial(logit))
summary(LogModel)

Px0=(exp(-3.7075))/(1+exp(-3.7075))
Px0
Px1=(exp(-3.7075+2.7912))/(1+exp(-3.7075+2.7912))
Px1

Oddsx0=exp(-3.7075)
Oddsx0
Oddsx1=exp(2.7912-3.7075)
Oddsx1

OR = exp(2.7912-3.7075)/exp(-3.7075)
OR

Beta = LogModel$coefficient[1]
Alpha = LogModel$coefficient[2]
Beta
Alpha

P0 <- exp(LogModel$coefficient[1]) / (1 + exp(LogModel$coefficient[1]))
P1 <- exp(LogModel$coefficient[1] + LogModel$coefficient[2]) / (1 + exp(LogModel$coefficient[1]+LogModel$coefficient[2]))
P0
P1

odds0 <- P0 / (1 - P0)
odds1 <- P1 / (1 - P1)
odds0
odds1

OR1 <- odds1 / odds0
OR1 

OR2 <- (TableDiabetes[1,1]*TableDiabetes[2,2]) / (TableDiabetes[1,2]*TableDiabetes[2,1])
OR2
