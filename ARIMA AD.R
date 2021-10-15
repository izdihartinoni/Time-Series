library(forecast)

fit<-auto.arima(bills$...2)
plot(forecast(fit,h=4))
summary(fit)

#ARIMA DATA TEMP ASIA BANGKOK 
library(forecast)
data=read.csv('*D:/no.2.csv')
data_nw<-data$BangkokMax
acf(data_nw,lag.max=30)
model<-auto.arima(data_nw)
model
auto.arima(data_nw,d=NA,max.p= 5, max.q= 5,max.Q= 2,max.order= 5, seasonal= TRUE)

#ARIMA SEASONAL
data_nw1= ts(data$BangkokMax, frequency = 12)
auto.arima(data_nw1, d=NA,D= NA,max.p= 5,max.q= 5,max.P= 5,max.Q= 2,max.order= 5, seasonal= TRUE)
fit<-auto.arima(data_nw1)
plot(forecast(fit,h=20))
ts.plot(data_nw1)

#ARIMA DATA NZBIRTH
library(forecast)
data2=read.csv('D:/NZBirths.csv')
data2_new<-data2$MaoriMale
acf(data2_new,lag.max=30)
model2<-auto.arima(data2_new)
summary(model2)

#ARIMA DATA NZaccomodation
library(forecast)
data3<-read.csv('D:/NZAccomodation.csv')
data3_new<-data3$Hotel
acf(data3_new,lag.max=30)
auto.arima(data3_new, d=NA,D= NA,max.p= 5,max.q= 5,max.P= 5,max.Q= 2,max.order= 5, seasonal= TRUE)
model2<-auto.arima(data3_new)
summary(model2)


y<-msts(x[1:1000],seasonal.periods= c(7,265,25))
fc= auto.arima(y, D=1, trace=T, stepwise=F)