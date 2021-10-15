library(lmtest)
library(tseries)
library(neuralnet)

data_NN <- read.csv("D:/KULIAH NONI/SEMESTER 7/ANALISIS DATA/DATA TIME SERIES.csv", sep = ";")
y <- data_NN[,3]
win.graph()
ts.plot(y)

#IDENTIFIKASI INPUT MENGGUNAKAN LAGPLOT
lag.plot(y, lags=16)

#IDENTIFIKASI INPUT MENGGUNAKAN PACF
par(mfrow=c(1,2))
acf(y, lag.max = 25)
pacf(y, lag.max = 25)

w <- diff(y, lag = 1)
acf(w, lag.max = 25)
pacf(w, lag.max = 25)

#PREPROCESSING STANDARDIZED#
mean.y=mean(y)
sd.y=sd(y)
y_std<-scale(y)
y_std_lag10=y_std[1:49]
y_std=y_std[11:59] 

data=data.frame(cbind(y_std,y_std_lag10))

#MODEL FFNN
model_FFNN=neuralnet(y_std~y_std_lag10,data=data, hidden=5,
              act.fct="tanh", linear.output=TRUE, likelihood=TRUE)
plot(model_FFNN)
fits.model_FFNN=(as.ts(unlist(model_FFNN$net.result)))*sd.y+mean.y

#MODEL DLNN
model_DLNN=neuralnet(y_std~y_std_lag10,data=data, hidden=c(5,3),
                   act.fct="tanh", linear.output=TRUE, likelihood=TRUE)
plot(model_DLNN)
fits.model_DLNN=(as.ts(unlist(model_DLNN$net.result)))*sd.y+mean.y
