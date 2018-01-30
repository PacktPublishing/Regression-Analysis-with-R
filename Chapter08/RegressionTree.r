data(mtcars)
str(mtcars)
summary(mtcars)


pairs(mpg~cyl+disp+hp+wt,data=mtcars)


library(tree)

RTModel <- tree(mpg~.,data = mtcars)
RTModel

summary(RTModel)


plot(RTModel)
text(RTModel)


