  #Read in Data
  M <- read.table('http://web.stanford.edu/~xing/statfinbook/_BookData/Chap03/m_sp500ret_3mtcm.txt', skip=1, header=T)
  
  X <- read.table('http://web.stanford.edu/~xing/statfinbook/_BookData/Chap03/m_ret_10stocks.txt', header=T)
  
  S <- read.table('http://web.stanford.edu/~xing/statfinbook/_BookData/Chap03/d_logret_6stocks.txt', header=T)
  formatC(apply(S[2:6],2,mean)*10^2,format="f",digits = 4)
  formatC(cov(S[2:6])*10^4,format="f",digits = 2)
  
  getSymbols("^DJI",from="2000-08-01", to="2005-10-03", return.class='data.frame')
  
  
  # Convert monthly prices into monthly returns
  r.monthly <- diff(log(monthly.prices))
  
  
