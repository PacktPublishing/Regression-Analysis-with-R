Nile<-as.data.frame(Nile)
summary(Nile)

plot(Nile)


library("arules")

NileDiscr<-discretize(Nile$x, method="interval", categories = 3,labels = c("Low","Med","High"),
                      ordered=FALSE, onlycuts=FALSE)

summary(NileDiscr)



HistNile<-hist(Nile$x, breaks =3,cex.axis=0.6,cex.lab=0.6,cex.main=0.6)

HistNile

HistNile2<-hist(Nile$x, breaks =c(456,760.67,1065.33,1370))
HistNile2