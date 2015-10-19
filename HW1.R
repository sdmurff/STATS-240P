## @knitr HW1point6
#### Exercise 1.6 ####
X <- read.table('http://web.stanford.edu/~xing/statfinbook/_BookData/Chap06/w_logret_3stocks.txt', header=T)
# Convert Date column from a factor to an R date
X[,1]<-as.Date(X[,1],"%m/%d/%Y")

## @knitr a1_6_1
#### 1.6 (a) ####
# Plot Pfizer returns and add lines to highlight behavior before and after March 8, 1999
plot(x=X$Date,y=X$PFE)
abline(v=as.Date('1999-03-08'))
abline(h=0)
## @knitr a1_6_2
# Box plot makes difference more apparent
boxplot(X$PFE[1:897],X$PFE[897:length(X$PFE)],names = c("Before March 8, 1999","After March 8, 1999"))
abline(h=0)

## @knitr b1_6_1
#### 1.6 (b) ####
# Create independent variables
X$less.than.t0<-ifelse(X$Date<as.Date('1999-03-08'),1,0)
X$more.than.t0<-ifelse(X$Date>=as.Date('1999-03-08'),1,0)
# Run regression and view output
fit.full<-lm(PFE ~ less.than.t0 + more.than.t0 - 1,data=X)
summary(fit.full)
## @knitr b1_6_2
confint(fit.full,level=0.95)

## @knitr c1_6_1
#### 1.6 (c) ####
# Fit the reduced model
X$x1.plus.x2<-X$less.than.t0+X$more.than.t0
fit.reduced<-lm(PFE ~ x1.plus.x2 - 1,data=X)
summary(fit.reduced)
# Use anova to carry out an F-test
anova(fit.reduced,fit.full)

## @knitr HW2point2
#### Exercise 2.2 ####
# Read in data from website
X <- read.table('http://web.stanford.edu/~xing/statfinbook/_BookData/Chap02/m_swap.txt', skip=1, header=T)

## @knitr a2_2_1
#### 2.2 (a) ####
# Do Manual PCA with covariance matrix
standardize<-function(x){(x-mean(x))}
# Standardize data. Since all variable are in the same units scaling by standard deviation is not necessary
X.standardized<-apply(X[2:length(X)],2,standardize)
X.covar<-cov(X.standardized)
X.eig<-eigen(X.covar)

# Use R function princomp to do PCA
cov.PCA<-princomp(X[2:length(X)])

# Compare manaul PCA with that from princomp
summary(cov.PCA)
# Standard deviation
formatC(sqrt(X.eig$values),format='f',digits = 6)
# Proportion of Variance
formatC(X.eig$values/sum(X.eig$values),format='f',digits = 6)
# Cumulative Proportion
formatC(cumsum(X.eig$values/sum(X.eig$values)),format='f',digits = 6)

# Plot the Variance
screeplot(cov.PCA)

## @knitr b2_2_1
#### 2.2 (b) ####
# Do Manual PCA with covariance matrix
X.corr<-cor(X.standardized)
X.eig<-eigen(X.corr)

# Use R function princomp to do PCA with correlation matrix
corr.PCA<-princomp(X[2:length(X)], cor=T)

# Compare manaul PCA with correlation matrix with that from princomp
summary(corr.PCA)
# Standard deviation
formatC(sqrt(X.eig$values),format='f',digits = 6)
# Proportion of Variance
formatC(X.eig$values/sum(X.eig$values),format='f',digits = 6)
# Cumulative Proportion
formatC(cumsum(X.eig$values/sum(X.eig$values)),format='f',digits = 6)

# Plot the variance
screeplot(corr.PCA)

## @knitr next
cor.PCA<-princomp(X[2:length(X)], cor=T)

## @knitr c2_2_1
#### 2.2 (c) ####
# Read in daily and monthly data in order to compare PCA results
D<-read.table("http://web.stanford.edu/~xing/statfinbook/_BookData/Chap02/d_swap.txt",skip=1,header=T)
M<-read.table('http://web.stanford.edu/~xing/statfinbook/_BookData/Chap02/m_swap.txt',skip=1,header=T)

#Replicate Results from Section 2.2.3
D.diff<-apply(D,2,diff)
D.diff.center<-scale(D.diff,center=T,scale=F)
cov.D.diff<-princomp(D.diff.center)
corr.D.diff<-princomp(D.diff.center,cor=T)
# Results from monthly swap file as calculated for (a) and (b) of this problem (not differenced)
M.center<-scale(M[2:length(M)],center=T,scale=F)
cov.M<-princomp(M.center)
corr.M<-princomp(M.center,cor=T)

# Comparsison 1: Results from Section 2.2.3 to results from non-differenced version of monthly swap data.
# PCA Comparison with Covariance Matrix
summary(cov.D.diff)
summary(cov.M)
# PCA Comparison with Correlation Matrix
summary(corr.D.diff)
summary(corr.M)

## @knitr c2_2_2
# Comparsison 2: Results from Section 2.2.3 to results from differenced version of monthly swap data.
# Results from monthly swap file after differencing
M.diff<-apply(M[2:length(M)],2,diff)
M.diff.center<-scale(M.diff,center=T,scale=F)
cov.M.diff<-princomp(M.diff.center)
corr.M.diff<-princomp(M.diff.center,cor=T)
# PCA Comparison with Covariance Matrix
summary(cov.D.diff)
summary(cov.M.diff)
# PCA Comparison with Correlation Matrix
summary(corr.D.diff)
summary(corr.M.diff)

## @knitr HW2point3
S <- read.table('http://stanford.edu/~xing/statfinbook/_BookData/Chap01/d_logret_12stocks.txt', header=T)
# Eliminate Date Column for simplicity
S[,1]<-NULL
S.center<-scale(S,center=T,scale=F)

## @knitr c2_3_1
#### Exercise 2.3 ####
#### 2.3 (a) ####
# Run PCA analysis using princomp function with Covariance Matrix
cov.S<-princomp(S.center)
summary(cov.S)

## @knitr c2_3_2
#### 2.3 (b) ####
# Run PCA analysis using princomp function with Correlation Matrix
corr.S<-princomp(S.center,cor=T)
summary(corr.S)

