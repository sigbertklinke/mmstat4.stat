############################################################################
################## Grafiken Testheorie ##################################
############################################################################

#Clear memory
rm(list=ls())

#install needed libraries
install.packages(c("reshape","foreign","plotrix"), dependencies=TRUE)
install.packages("Hmisc", dependencies=TRUE)

#Loading Libraries
library(foreign)
library(reshape)
library(foreach)
library(Hmisc)
library(Design)
library(MASS)
library(plotrix)
library(mvtnorm)
library(scatterplot3d)
library(rgl)

#setwd("C:/Documents and Settings/franzi.schulz/My Documents/BEIDS")


### Balkendiagramm B(30,0.2)
x <- seq(3,10,by=1)
x1<- seq(0,2,by=1)
y <- dbinom(x,30,0.2)
y1<- dbinom(x1,30,0.2)

pdf("Binom30.pdf")
par(bg="transparent")
plot(x,y,type="h", xlim=c(-1,10), ylim=c(0,0.22), lwd=4, xlab="X", ylab=" ") 
lines(x1,y1, type="h",col="red", lwd=4)

#par(bg="transparent")
boxed.labels(x=1,y=0.2, ypad=0.9,expression(Ablehnung),border=FALSE,cex=1.2, col="red",bg="transparent")
boxed.labels(x=5.5,y=0.2, ypad=0.9,expression(Nichtablehnung),border=FALSE,cex=1.2,bg="transparent")
dev.off()

#######################################################################################
## B(100,0.5)

x <- seq(10,90,by=1)
x1 <- seq(30,42,by=1)
x2 <- seq(58,70,by=1)
y <- dbinom(x,100,0.5)
y1 <- dbinom(x1,100,0.5)
y2 <- dbinom(x2,100,0.5)

pdf("Binom2.pdf", height=1, width=4)
par(mar=c(0,0,0,0),bg="transparent")
plot(x,y,type="h",xlim=c(30,70) , ylim=c(0,0.10), lwd=2, xlab=" ", ylab=" " , frame.plot=FALSE, axes=F)
lines(x1,y1, type="h",col="red", lwd=2)
lines(x2,y2, type="h",col="red", lwd=2)
dev.off()


x <- seq(10,100,by=1)
x1 <- seq(30,42,by=1)
y <- dbinom(x,100,0.5)
y1 <- dbinom(x1,100,0.5)

pdf("BinomL.pdf", height=1, width=4)
par(mar=c(0,0,0,0),bg="transparent")
plot(x,y,type="h",xlim=c(30,70) , ylim=c(0,0.10), lwd=2, xlab=" ", ylab=" " , frame.plot=FALSE, axes=F)
lines(x1,y1, type="h",col="red", lwd=2)
dev.off()


x <- seq(0,90,by=1)
x2 <- seq(58,70,by=1)
y <- dbinom(x,100,0.5)
y1 <- dbinom(x1,100,0.5)

pdf("BinomR.pdf", height=1, width=4)
par(mar=c(0,0,0,0),bg="transparent")
plot(x,y,type="h",xlim=c(30,70) , ylim=c(0,0.10), lwd=2, xlab=" ", ylab=" " , frame.plot=FALSE, axes=F)
lines(x2,y2, type="h",col="red", lwd=2)
dev.off()



###################################################



cz <- 1.96
ce <- 1.65

x <- (-350:350)/100
y <- dnorm(x)
#
#pdf("zweiseitig.pdf", height=1, width=4)
par(bg="transparent")
plot(x,y, type="l", xlab="", ylab="", ylim=c(-0.04,0.41), yaxt="n", xaxt="n")
lines(c(min(x), max(x)), c(0,0))
ablineclip(v=0, y1=0, y2=dnorm(0),untf = FALSE )



draw.polygon <- function (x, from, to, col, d=0) {
  xp <- seq(from, to, length.out=sum((x<=to) & (x>=from)))
  yp <- dnorm(xp, d)
  polygon(c(from, xp, to), c(0, yp, 0), col=col) 
}

draw.polygon(x, min(x), -cz, "red") 
draw.polygon(x, cz, max(x), "red") 

boxed.labels(x=0,y=-0.02, xpad=0.5, ypad=0.9,expression(0),border=FALSE,cex=1.2,bg="transparent")
boxed.labels(x=-1.97,y=-0.02, xpad=0.5, ypad=0.9,expression(-z[1-frac(alpha,2)]),border=FALSE,cex=1.2,bg="transparent")
boxed.labels(x=+1.95,y=-0.02, xpad=0.5, ypad=0.9,expression(+z[1-frac(alpha,2)]),border=FALSE,cex=1.2,bg="transparent")
#dev.off()

### Verteilung Xquer

cz <- 1.96*1.3
ce <- 1.65

x <- (-350:350)/100
y <- dnorm(x,0,1.3)
#

par(bg="transparent")
plot(x,y, type="l", xlab="", ylab="", ylim=c(-0.04,0.41), xaxt="n",yaxt="n")
lines(c(min(x), max(x)), c(0,0))
ablineclip(v=0, y1=0, y2=dnorm(0,0,1.3),untf = FALSE )



draw.polygon <- function (x, from, to, col) {
  xp <- seq(from, to, length.out=sum((x<=to) & (x>=from)))
  yp <- dnorm(xp, 0,1.3)
  polygon(c(from, xp, to), c(0, yp, 0), col=col) 
}

draw.polygon(x, min(x), -cz, "red") 
draw.polygon(x, cz, max(x), "red") 

boxed.labels(x=0,y=-0.02, xpad=0.5, ypad=0.9,expression(mu[0]),border=FALSE,cex=1.2,bg="transparent")
boxed.labels(x=-2.6,y=-0.025, xpad=0.5, ypad=0.9,expression(mu[0]-z[1-frac(alpha,2)]*" "*frac(sigma,sqrt(n))),border=FALSE,cex=1.19,bg="transparent")
boxed.labels(x=+2.5,y=-0.025, xpad=0.5, ypad=0.9,expression(mu[0]+z[1-frac(alpha,2)]*" "*frac(sigma,sqrt(n))),border=FALSE,cex=1.19,bg="transparent")


### G?tefunktion

# Beidseitiger Test mu0=1000, sigma=1.5 n=100
x<-seq(988,1012,by=0.01)
y=1-(pnorm(1.96-(x-1000)/1.7)-pnorm(-1.96-(x-1000)/1.7))
plot(x,y, type="l", ylab=" ", xlab=expression(mu),cex.lab=1.2)
abline(v=1000,lty=3)
abline(v=997, lty=2)
abline(v=1006, lty=2)

boxed.labels(x=995.7,y=0.2,xpad=0.5, ypad=0.9,expression(1-beta),border=FALSE,cex=1.2,bg="transparent")
boxed.labels(x=996.5,y=0.9,xpad=0.5, ypad=0.9,expression(beta),border=FALSE,cex=1.2,bg="transparent")

boxed.labels(x=1000.8,y=0.04,xpad=0.5, ypad=0.9,expression(alpha),border=FALSE,cex=1.2,bg="transparent")
boxed.labels(x=1001.2,y=0.6,xpad=0.9, ypad=0.9,expression(1-alpha),border=FALSE,cex=1.2,bg="transparent")

boxed.labels(x=1004.7,y=0.2,xpad=0.5, ypad=0.9,expression(1-beta),border=FALSE,cex=1.2,bg="transparent")
boxed.labels(x=1005.5,y=0.96,xpad=0.5, ypad=0.9,expression(beta),border=FALSE,cex=1.2,bg="transparent")


# Rechtsseitiger Test 

x<-seq(988,1012,by=0.01)
y=1-(pnorm(1.64-(x-1000)/1.7))
plot(x,y, type="l", ylab=" ", xlab=expression(mu),cex.lab=1.2)
abline(v=1000,lty=3)
#abline(v=997, lty=2)
abline(v=1005, lty=2)

#boxed.labels(x=995.7,y=0.2,xpad=0.5, ypad=0.9,expression(1-beta),border=FALSE,cex=1.2,bg="transparent")
#boxed.labels(x=996.5,y=0.9,xpad=0.5, ypad=0.9,expression(beta),border=FALSE,cex=1.2,bg="transparent")

boxed.labels(x=1000.8,y=0.04,xpad=0.5, ypad=0.9,expression(alpha),border=FALSE,cex=1.2,bg="transparent")
boxed.labels(x=1001.2,y=0.6,xpad=0.9, ypad=0.9,expression(1-alpha),border=FALSE,cex=1.2,bg="transparent")

boxed.labels(x=1003.9,y=0.2,xpad=0.5, ypad=0.9,expression(1-beta),border=FALSE,cex=1.2,bg="transparent")
boxed.labels(x=1004.6,y=0.96,xpad=0.5, ypad=0.9,expression(beta),border=FALSE,cex=1.2,bg="transparent")


# Linksseitiger Test 

x<-seq(988,1012,by=0.01)
y=(pnorm(-1.64-(x-1000)/1.7))
plot(x,y, type="l", ylab=" ", xlab=expression(mu),cex.lab=1.2)
abline(v=1000,lty=3)
#abline(v=997, lty=2)
abline(v=995, lty=2)

#boxed.labels(x=995.7,y=0.2,xpad=0.5, ypad=0.9,expression(1-beta),border=FALSE,cex=1.2,bg="transparent")
#boxed.labels(x=996.5,y=0.9,xpad=0.5, ypad=0.9,expression(beta),border=FALSE,cex=1.2,bg="transparent")

boxed.labels(x=999.3,y=0.025,xpad=0.5, ypad=0.9,expression(alpha),border=FALSE,cex=1.2,bg="transparent")
boxed.labels(x=998.8,y=0.6,xpad=0.9, ypad=0.9,expression(1-alpha),border=FALSE,cex=1.2,bg="transparent")

boxed.labels(x=996.1,y=0.2,xpad=0.5, ypad=0.9,expression(1-beta),border=FALSE,cex=1.2,bg="transparent")
boxed.labels(x=995.6,y=0.96,xpad=0.5, ypad=0.9,expression(beta),border=FALSE,cex=1.2,bg="transparent")

### Gütefunktion verschiedene n


# Beidseitiger Test mu0=1000, sigma=1.5 n=100
x<-seq(988,1012,by=0.01)
y=1-(pnorm(1.96-(x-1000)/1.7)-pnorm(-1.96-(x-1000)/1.7))
y1=1-(pnorm(1.96-(x-1000)/0.8)-pnorm(-1.96-(x-1000)/0.8))
y2=1-(pnorm(1.96-(x-1000)/1.3)-pnorm(-1.96-(x-1000)/1.3))
y3=1-(pnorm(1.96-(x-1000)/1.1)-pnorm(-1.96-(x-1000)/1.1))

par(bg="transparent")
plot(x,y, type="l", ylab=" ", xlab=" ",cex.lab=1.2, lwd=2, xaxt="n")
lines(x,y1, col="mediumblue", lwd=2)
lines(x,y2,col="mediumpurple4", lwd=2)
lines(x,y3, col="violetred4",lwd=2)

axis(1, at=1000, labels=expression(~~mu[0]), cex=1.7)
abline(v=1000,lty=2)

boxed.labels(x=1011,y=0.51,xpad=0.5, ypad=0.9,expression(n[1]),border=FALSE,cex=1.2,bg="transparent")
boxed.labels(x=1011,y=0.44,xpad=0.5, ypad=0.9,expression(n[2]),border=FALSE,cex=1.2,bg="transparent",col="mediumpurple4")
boxed.labels(x=1011,y=0.37,xpad=0.5, ypad=0.9,expression(n[3]),border=FALSE,cex=1.2,bg="transparent",col="violetred4")
boxed.labels(x=1011,y=0.3,xpad=0.5, ypad=0.9,expression(n[4]),border=FALSE,cex=1.2,bg="transparent",col="mediumblue")


### Gütefunktio 2 alpha

x<-seq(988,1012,by=0.01)
y=1-(pnorm(1.96-(x-1000)/1.7)-pnorm(-1.96-(x-1000)/1.7))
y1=1-(pnorm(1.64-(x-1000)/1.7)-pnorm(-1.64-(x-1000)/1.7))

par(bg="transparent")
plot(x,y, type="l", ylab=" ", xlab=" ",cex.lab=1.2, lwd=2, xaxt="n")
lines(x,y1,lwd=2,col="mediumblue")

axis(1, at=1000, labels=expression(~~mu[0]), cex=1.7)
abline(v=1000,lty=2)

boxed.labels(x=1010.5,y=0.25,xpad=0.5, ypad=0.9,expression(alpha[1]==0.05),border=FALSE,cex=1.2,bg="transparent")
boxed.labels(x=1010.3,y=0.17,xpad=0.5, ypad=0.9,expression(alpha[2]==0.1),border=FALSE,cex=1.2,bg="transparent",col="mediumblue")


### Gütefunktion Anteilswert

x<-seq(0,1,by=0.01)
y=pbinom(1,10,x) + (1-pbinom(8,10,x))

par(bg="transparent")
plot(x,y, type="l", ylab=" ", xlab=expression(pi),cex.lab=1.2, lwd=2)

abline(v=0.5,lty=3)
abline(v=0.2, lty=2)
abline(v=0.6, lty=2)

boxed.labels(x=0.15,y=0.2,xpad=0.5, ypad=0.9,expression(1-beta),border=FALSE,cex=1.2,bg="transparent")
boxed.labels(x=0.18,y=0.9,xpad=0.5, ypad=0.9,expression(beta),border=FALSE,cex=1.2,bg="transparent")

boxed.labels(x=0.475,y=0.004,xpad=0.5, ypad=0.9,expression(alpha),border=FALSE,cex=1.2,bg="transparent")
boxed.labels(x=0.45,y=0.6,xpad=0.9, ypad=0.9,expression(1-alpha),border=FALSE,cex=1.2,bg="transparent")

boxed.labels(x=0.64,y=0.02,xpad=0.5, ypad=0.9,expression(1-beta),border=FALSE,cex=1.2,bg="transparent")
boxed.labels(x=0.62,y=0.9,xpad=0.5, ypad=0.9,expression(beta),border=FALSE,cex=1.2,bg="transparent")

