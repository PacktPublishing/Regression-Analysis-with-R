data = UCBAdmissions
str(data)
summary(data)

TotalAdmit=apply(data, c(1, 2), sum)

barplot(TotalAdmit,legend = rownames(TotalAdmit),cex.axis=0.6,cex.lab=0.6,cex.names=0.6,cex=0.6)

ODTotal=TotalAdmit[1,1]*TotalAdmit[2,2]/(TotalAdmit[1,2]*TotalAdmit[2,1])
chisq.test(TotalAdmit,correct=FALSE)
data

odds.ratio=function(x,addtocounts=0){
  x=x+addtocounts
  (x[1,1]*x[2,2])/(x[1,2]*x[2,1])
}
OD<-round(apply(data,1,odds.ratio),2);OD

AdmitRates<-round(prop.table(margin.table(UCBAdmissions,c(1,3)),2),2)

par(mfrow=c(2,3))
for (i in 1:6) 
  barplot(data[,,i],legend.text=FALSE,main=paste("Dept",LETTERS[i],sep=" "))

DeptResults <- prop.table(data,c(2,3))
DeptResults
DeptResults[1,,]

FirstTwoColumn<-matrix(data,ncol=2,byrow=TRUE,dimnames=list(NULL,c("Admitted","Rejected")))
Gender<-rep(c("Male","Female"),6)
Dept<-rep(c("A","B","C","D","E","F"),each=2)
data1 <- data.frame(FirstTwoColumn,Gender,Dept)


MLogit1=glm(cbind(Admitted,Rejected)~Gender+Dept,family=binomial(link=logit),data=data1)
#             contrasts=list(Dept=contr.treatment(6,base=6,contrasts=TRUE)))
summary(MLogit1)

MLogit2=glm(cbind(Admitted,Rejected)~Gender+Dept,family=binomial(link=logit),data=data1,subset = (Dept != "A"))
summary(MLogit2)


MLogit3=glm(cbind(Admitted,Rejected)~Dept,family=binomial(link=logit),data=data1,subset = (Dept != "A"))
summary(MLogit3)

