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

# Exercise 5.7 (a)
# Read in data from website
Y <- read.table('http://web.stanford.edu/~xing/statfinbook/_BookData/Chap05/w_logret_yahoo.txt', header=T)
# Create R compatable date column
Y[,1]<-as.Date(Y[,1],format="%m/%d/%Y")
# Calculate the week of the year, 1-52,53
Y$week<-week(Y$Date)
# Exclude the 53rd weeks
Y<-Y[Y$week !=53,]
# Create Time Series object for input into stl function
Y.ts<-ts(Y$logret,freq=52, start=1996+14/52, end=2007+25/52)
# Use stl function to decompose series into trend, seasonal, and residual components
Y.season<-stl(Y.ts,s.window="periodic")
plot(Y.season)

# Calulate proportion of variance for each series component
apply(Y.season$time.series, 2, var) / var(Y.ts)

# Exercise 5.7 (b)
# Read in data from website
Y <- read.table('http://web.stanford.edu/~xing/statfinbook/_BookData/Chap05/w_logret_yahoo.txt', header=T)
# Create R compatable date column
Y[,1]<-as.Date(Y[,1],format="%m/%d/%Y")
# Run the Ljung-Box test for lags 1-10 to determine if there are serial correlations in the series
lb<-rep(1,10)
for(i in 1:10){
    p<-Box.test(Y$logret,lag=i,type="Ljung-Box")
    lb[i]<-p$p.value
}
names(lb)<-c("pval.Lag 1","pval.Lag 2","pval.Lag 3","pval.Lag 4","pval.Lag 5",
             "pval.Lag 6","pval.Lag 7","pval.Lag 8","pval.Lag 9","pval.Lag 10")
lb


plot(lb,xlab="Lag", ylab="p-value",ylim=c(0,.5))
abline(h=.05)



