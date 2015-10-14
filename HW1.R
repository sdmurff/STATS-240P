# Exercise 1.6
X <- read.table('http://web.stanford.edu/~xing/statfinbook/_BookData/Chap06/w_logret_3stocks.txt', header=T)

X[,1]<-as.Date(X[,1],"%m/%d/%Y")

glimpse(X)
plot(x=X$Date,y=X$PFE)
abline(v=as.Date('1999-03-08'))
abline(h=0)


ggplot(X, aes(x=X$Date, y=X$PFE)) + 
          geom_point(shape=1) +
          geom_vline(xintercept=as.numeric((as.Date('1999-03-08'))), linetype="dotted")

summary(X$PFE[1:897])
summary(X$PFE[898:length(X$PFE)])

hist(X$PFE[898:length(X$PFE)])


# Problem 2.2
X <- read.table('http://web.stanford.edu/~xing/statfinbook/_BookData/Chap02/m_swap.txt', skip=1, header=T)

summary(princomp(X[,-1]))
summary(princomp(X[,-1], cor=T))






# Author: Steve Pittard - wsp@emory.edu, ticopittard@gmail.com
# This code is in support of the the following two YOUTUBE videos which attempt to explain the basics of PCA

# Principal Components Analysis Using R - P1 http://www.youtube.com/watch?v=5zk93CpKYhg
# Principal Components Analysis Using R - P2 http://www.youtube.com/watch?v=I5GxNzKLIoU 
 

library(calibrate)
my.classes = read.csv("http://steviep42.bitbucket.org/YOUTUBE.DIR/marks.dat")
plot(my.classes,cex=0.9,col="blue",main="Plot of Physics Scores vs. Stat Scores")
options(digits=3)
par(mfrow=c(1,1))

# Scale the data

standardize <- function(x) {(x - mean(x))}
my.scaled.classes = apply(my.classes,2,function(x) (x-mean(x)))
my.scaled.classes1 = apply(my.classes,2,standardize)
plot(my.scaled.classes,cex=0.9,col="blue",main="Plot of Physics Scores vs. Stat Scores",sub="Mean Scaled",xlim=c(-30,30))

# Find Eigen values of covariance matrix

my.cov = cov(my.scaled.classes)
my.eigen = eigen(my.cov)
rownames(my.eigen$vectors)=c("Physics","Stats")
colnames(my.eigen$vectors)=c("PC1","PC")
# Note that the sum of the eigen values equals the total variance of the data

sum(my.eigen$values)
var(my.scaled.classes[,1]) + var(my.scaled.classes[,2])

# The Eigen vectors are the principal components. We see to what extent each variable contributes

loadings = my.eigen$vectors

# Let's plot them 

pc1.slope = my.eigen$vectors[1,1]/my.eigen$vectors[2,1]
pc2.slope = my.eigen$vectors[1,2]/my.eigen$vectors[2,2]

abline(0,pc1.slope,col="red")
abline(0,pc2.slope,col="green")

textxy(12,10,"(-0.710,-0.695)",cx=0.9,dcol="red")
textxy(-12,10,"(0.695,-0.719)",cx=0.9,dcol="green")
 
# See how much variation each eigenvector accounts for

pc1.var = 100*round(my.eigen$values[1]/sum(my.eigen$values),digits=2)
pc2.var = 100*round(my.eigen$values[2]/sum(my.eigen$values),digits=2)
xlab=paste("PC1 - ",pc1.var," % of variation",sep="")
ylab=paste("PC2 - ",pc2.var," % of variation",sep="")

# Multiply the scaled data by the eigen vectors (principal components)

scores = my.scaled.classes %*% loadings
sd = sqrt(my.eigen$values)
rownames(loadings) = colnames(my.classes)

plot(scores,ylim=c(-10,10),main="Data in terms of EigenVectors / PCs",xlab=xlab,ylab=ylab)
abline(0,0,col="red")
abline(0,90,col="green")

# Correlation BiPlot

scores.min = min(scores[,1:2])
scores.max = max(scores[,1:2])

plot(scores[,1]/sd[1],scores[,2]/sd[2],main="My First BiPlot",xlab=xlab,ylab=ylab,type="n")
rownames(scores)=seq(1:nrow(scores))
abline(0,0,col="red")
abline(0,90,col="green")

# This is to make the size of the lines more apparent
factor = 5

# First plot the variables as vectors
arrows(0,0,loadings[,1]*sd[1]/factor,loadings[,2]*sd[2]/factor,length=0.1, lwd=2,angle=20, col="red")
text(loadings[,1]*sd[1]/factor*1.2,loadings[,2]*sd[2]/factor*1.2,rownames(loadings), col="red", cex=1.2)

# Second plot the scores as points
text(scores[,1]/sd[1],scores[,2]/sd[2], rownames(scores),col="blue", cex=0.7)

somelabs = paste(round(my.classes[,1],digits=1),round(my.classes[,2],digits=1),sep=" , ")
#identify(scores[,1]/sd[1],scores[,2]/sd[2],labels=somelabs,cex=0.8)