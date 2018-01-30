Airquality<-as.data.frame(airquality)
summary(Airquality)

Airquality<-na.omit(Airquality)

max_data <- apply(Airquality, 2, max)
min_data <- apply(Airquality, 2, min)
data_scaled1 <- scale(Airquality,center = min_data, scale = max_data - min_data)
summary(data_scaled1)

mean_data <- apply(Airquality, 2, mean)
sd_data <- apply(Airquality, 2, sd)
data_scaled2 <- scale(Airquality,center = mean_data, scale = sd_data)
summary(data_scaled2)

SDdata_scaled2<-apply(data_scaled2,2,sd)
SDdata_scaled2
