library(dplyr)
library(stargazer)
library(texreg)
library(xtable)
#Read in Data
M <- read.table('http://web.stanford.edu/~xing/statfinbook/_BookData/Chap03/m_sp500ret_3mtcm.txt', skip=1, header=T)
S <- read.table('http://web.stanford.edu/~xing/statfinbook/_BookData/Chap03/m_ret_10stocks.txt', header=T)

#### DATA PREPARATION
#convert 3 month t-bill rate to a decimal and then to a monthly rate to be comparable to market and stock returns.
M$treas3mo_monthly<-((M$X3mTCM/100)+1)^(1/12)-1

#Compute market and individual stock excess returns.
excess.market<-M$sp500-M$treas3mo_monthly
excess.stocks<-as.data.frame(apply(S[-1],2,function(x){x-M$treas3mo_monthly}))

#Run CAPM with Jensen index (alpha)
capm0<-lm(excess.stocks[,1]~excess.market)

capm<-lapply(excess.stocks,function(x){lm(x~excess.market)})
stargazer(capm)

#Compute the Sharp Ratio
sharpe<-apply(excess.stocks,2,mean)/apply(S[-1],2,sd)
sharpe1<-paste0(formatC(sharpe*10^2,digits=2,format="f"))

beta<-NULL
for(i in 1:10){
    beta<-c(beta,capm[[i]]$coefficients[2])
}
treynor<-apply(excess.stocks,2,mean)/beta
treynor1<-paste0(formatC(treynor*10^3,digits=2,format="f"))

