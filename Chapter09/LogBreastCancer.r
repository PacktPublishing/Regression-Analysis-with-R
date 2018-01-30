BCData <- read.table(url("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"), sep = ",")
names(BCData)<- c('Id', 'ClumpThickness', 'CellSize','CellShape',      
                  'MarginalAdhesion','SECellSize', 'BareNuclei',
                  'BlandChromatin','NormalNucleoli', 'Mitoses','Class') 
str(BCData)

BCData<-BCData[!(BCData$BareNuclei=="?"),]
BCData$BareNuclei<-as.integer(BCData$BareNuclei)
str(BCData)
summary(BCData)

table(BCData$Class)


boxplot(BCData[,2:10])



par(mfrow=c(3, 3))
hist(BCData$ClumpThickness)
hist(BCData$CellSize)
hist(BCData$CellShape)
hist(BCData$MarginalAdhesion)
hist(BCData$SECellSize)
hist(BCData$BareNuclei)
hist(BCData$BlandChromatin)
hist(BCData$NormalNucleoli)
hist(BCData$Mitoses)
par(mfrow=c(1, 1))

BCData$Class<-replace(BCData$Class,BCData$Class==2,0)
BCData$Class<-replace(BCData$Class,BCData$Class==4,1)

table(BCData$Class)

LoGModel <- glm(Class ~.-Id,family=binomial(link='logit'),data=BCData)
summary(LoGModel)

anova(LoGModel, test="Chisq")

LGModelPred <- round(predict(LoGModel, type="response"))
table(LGModelPred)

table(BCData$Class,LGModelPred)

library(caret) 
confusionMatrix(LGModelPred,BCData$Class,positive="1")

library(pROC)

RocObj<-roc(BCData$Class,LGModelPred)

par(mfrow=c(1, 1))
plot.roc(RocObj)



plot(RocObj, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="blue", print.thres=TRUE)


