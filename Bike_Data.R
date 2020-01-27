### import the data

dt<- read.csv(file.choose(),header = T)
class(dt)
head(dt)
str(dt)
View(dt)

####convert date in Date format

dt$date <- as.character(dt$dteday,"%d/%m/%y")
str(dt)

### deseasonlise data

count_ma <- ts(na.omit(dt$cnt),frequency = 30)
class(count_ma)
View(count_ma)
### decompose the data

decomp <- stl(count_ma,s.window = "periodic")
plot(decomp)
deseasonal_cnt <- seasadj(decomp)

plot(decomp)

plot(count_ma)
plot(deseasonal_cnt)

adf.test(deseasonal_cnt,alternative = "stationary")

##### since the data is not stationary, we will do differencing

count_d1<- diff(deseasonal_cnt,differences = 2)

plot(count_d1)

adf.test(count_d1,alternative = "stationary")


##### auto corelation

acf(count_d1,main="acf plot for the series")
pacf(count_d1,main="pacf plot for the series")

a<- auto.arima(deseasonal_cnt)
a

###fit

auto <- auto.arima(deseasonal_cnt)

fit_bike<-  arima(log(deseasonal_cnt),c(1,1,1),
              seasonal =list(order=c(0,0,1),period=30))

fit<- arima(deseasonal_cnt,order = c(0,1,7))



### predict

fcast<- forecast(fit_bike,h=30)

plot(fcast)


#### since the forecast is flatlined, we need to include the seasonality which we removed in the first place

fit_seas<- auto.arima(deseasonal_cnt,seasonal = TRUE)

seas_fcast <- forecast(fit_seas,h=30)

plot(seas_fcast)

accuracy(seas_fcast)

