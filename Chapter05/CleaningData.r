setwd ("c://R")
SampleData=read.csv ("CleaningData.csv", header=TRUE,sep=";")
head(SampleData)
summary(SampleData)
SampleData

str(SampleData)
SampleData$right<-as.numeric(levels(SampleData$right))[SampleData$right]
SampleData

SampleData[SampleData==""]<-NA
summary(SampleData)


SampleData$right[SampleData$right == -19 ] <- NA
SampleData$right

is.na(SampleData)
which (is.na(SampleData))
which (is.na(SampleData), arr.ind=TRUE)

unique(unlist(lapply(SampleData, function (x) which (is.na (x)))))

SampleDataMinor<-na.omit(SampleData)
summary(SampleDataMinor)

is.nan(SampleData$right)

boxplot(SampleDataMinor,cex.axis=0.6,cex.lab=0.6)


boxplot.stats(SampleDataMinor$age)
