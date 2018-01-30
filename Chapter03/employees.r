library(readxl)

setwd ("c://R")
EmployeesSalary <- read_excel("employees.xlsx")

summary(EmployeesSalary)

EmployeesSalary$LevelOfEmployee <- as.factor(EmployeesSalary$LevelOfEmployee)

summary(EmployeesSalary)

pch.list <- as.numeric(EmployeesSalary$LevelOfEmployee)
plot(EmployeesSalary$YearsExperience, EmployeesSalary$Salary, pch=c(pch.list))

LMcat <- lm('Salary~YearsExperience*LevelOfEmployee',data=EmployeesSalary)
LMcat

LMcat.coef <- coef(LMcat)
LMcat.coef
LMcat.GeneralStaff <- c(LMcat.coef[1],LMcat.coef[2])
LMcat.GeneralStaff 
LMcat.TechnicalStaff <- c(LMcat.coef[1]+LMcat.coef[4],LMcat.coef[2]+LMcat.coef[6])
LMcat.TechnicalStaff
LMcat.Management <- c(LMcat.coef[1]+LMcat.coef[3],LMcat.coef[2]+LMcat.coef[5])
LMcat.Management

pch.list <- as.numeric(EmployeesSalary$LevelOfEmployee)
plot(EmployeesSalary$YearsExperience, EmployeesSalary$Salary, pch=c(pch.list),cex.axis=0.6,cex.lab=0.6)
abline(coef=LMcat.GeneralStaff,lwd=2)
abline(coef=LMcat.TechnicalStaff,lwd=2)
abline(coef=LMcat.Management,lwd=2)

summary(LMcat)
