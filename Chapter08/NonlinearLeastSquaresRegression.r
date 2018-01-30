x<-seq(0,200,1)
y<-((runif(1,8,25)*x)/(runif(1,0,7)+x))+runif(201,0,1)


plot(x,y)


LModel<-lm(y~x)
LMSummary<-summary(LModel)
LMSummary

NLModel<-nls(y~a*x/(b+x),start=list(a=1,b=0.1))
NLMSummary<-summary(NLModel)
NLMSummary

LMSummary$sigma
NLMSummary$sigma

plot(x,y)
abline(LModel)
lines(x,predict(NLModel),col="red")



