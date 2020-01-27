### read data

data("AirPassengers")


install.packages("tseries")
install.packages("forecast")
update.packages("tseries")

installed.packages()

library(tseries)
library(ggplot2)
library(forecast)

class(AirPassengers)


#EDA

start(AirPassengers)
end(AirPassengers)

frequency(AirPassengers)
plot(AirPassengers)

abline(reg = lm(AirPassengers~time(AirPassengers)))

### trend

plot(aggregate(AirPassengers,FUN = mean))

cycle(AirPassengers)

boxplot(AirPassengers~cycle(AirPassengers))


#### convert in to timeseries

tsdata <- ts(AirPassengers, frequency = 12)

### decompose

plot(decompose(AirPassengers))

#### data has trend and is not stationary
### convert to stationary


plot(diff(log(AirPassengers)))


###   AR I MA
##### p  d  q
#### Auto regression, INtegration, Moving Average


### acf with no differencing
acf(AirPassengers)   ### gives the value of Q


#### acf with differencing

acf(diff(log(AirPassengers)))   ### gives the value of Q


#### pacf with differencing

pacf(diff(log(AirPassengers)))   ### gives the value of P


### fit  the model
fit<- arima(log(AirPassengers),c(0,1,1),seasonal = list(order=c(0,1,1),period=12))

pred<- predict(fit,n.ahead=10*12)

### since it is in log form, convert it in decimal form

pred1<- 2.718^pred$pred

ts.plot(AirPassengers,2.718^pred$pred,log="y",lty=c(1,3))

#### test our model

datawide <- ts(AirPassengers,frequency = 12, start = c(1949,1),end = c(1959,12))


fit1<-  arima(log(datawide),c(0,1,1),seasonal =list(order=c(0,1,1),period=12))

pred2<- predict(fit1,n.ahead=10*12)

pred2<- 2.718^pred2$pred

data1<-head(pred2,12)

### round it off

predicted_1960 <- round(data1,digits = 0)

original_1960 <- tail(AirPassengers,12)

