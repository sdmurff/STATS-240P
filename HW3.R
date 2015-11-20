library(car)
library(timeseries)
library(forecast)
library(xts)
library(lubridate)
library(chron)
Y <- read.table('http://web.stanford.edu/~xing/statfinbook/_BookData/Chap05/w_logret_yahoo.txt', header=T)

S<-read.table('http://web.stanford.edu/~xing/statfinbook/_BookData/Chap03/d_logret_6stocks.txt', header=T)
S[,1]<-as.Date(S[,1],format="%d-%b-%y")

plot(T,type='l')

pairs()
#Convert Date to R Date Values
Y[,1]<-as.Date(Y[,1],format="%d-%b-%y")

plot(Y,type="l")
abline(h=0)

# Replicate Example 5.2.2 from Textbook
D <- read.table('http://web.stanford.edu/~xing/statfinbook/_BookData/Chap05/unem_dallas.txt', header=T,skip=1)
D <- ts(D$rate, start=c(1980, 1), end=c(2005, 6), frequency=12)
plot(D)
acf(D)
acf(D$rate)
pacf(D)
training <- window(D, start=c(1980, 1), end=c(2004, 12))
acf(D)
pacf(D)
training.season<-stl(training,s.window="periodic")
monthplot(training.season)
plot(training.season)
training.deseason<-training.season$time.series[,2]+training.season$time.series[,3]
arma<-arima(training.deseason,order=c(1,0,1),method="ML")
arma
predict(arma,n.ahead=6)
arma.resid<-arma$residuals
a<-Box.test(arma.resid,lag=10,type="Ljung-Box")
acf(deseason)
pacf(deseason)

apply.monthly(training.season$time.series[,1],mean)


auto.arima(training.deseason,ic="aic")


auto.arima(D.season$time.series[,3])
D[,1]<-as.Date(D[,1],format="%b-%b-%y")

# Exercise 5.7
Y <- read.table('http://web.stanford.edu/~xing/statfinbook/_BookData/Chap05/w_logret_yahoo.txt', header=T)
Y <- read.table('http://web.stanford.edu/~xing/statfinbook/_BookData/Chap05/w_logret_yahoo.txt', header=T)
Y[,1]<-as.Date(Y[,1],format="%m/%d/%Y")
# Y <- zoo(Y$logret,Y$Date)
# plot(Y)
# 
# Y <- ts(coredata(Y), freq = 52)
# Create column that tells me the week of the year
Y$week<-week(Y$Date)
Y$yday<-yday(Y$Date)
Y<-Y[Y$week !=53,]
Y.ts<-ts(Y$logret,freq=52, start=1996+14/52, end=2007+25/52)
plot(Y.ts)

Y.season<-stl(Y.ts,s.window="periodic")
apply(Y.season$time.series, 2, var) / var(Y.ts)
plot(Y.season)
str(Y)
Y$week<-week(Y$Date)
