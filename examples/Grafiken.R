############################################################################
################## Grafiken Sch?tztheorie ##################################
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

# MSE
x<-1.5
y<-0.5
plot(x,y,type="p", pch=16, xlim=c(1.2,2.8), ylim=c(0,2), cex=2.5, ylab=".",xlab=" ",
     cex.axis=1.3,cex.lab=1.3,xaxt="n")
mtext(side = 2, text = "MSE", line = 2.5,cex=1.3)

lines(2,1.4, lwd=3,type="p",pch=16, cex=2.5,col="aquamarine4")
lines(2.5,1.1, lwd=3,type="p",pch=16, cex=2.5, col="mediumblue")

boxed.labels(x=1.52,y=0.23, xpad=0.5, ypad=0.9,expression(hat(theta)^(1)),border=FALSE,cex=1.3)
boxed.labels(x=2.02,y=1.13, xpad=0.5, ypad=0.9,expression(hat(theta)^(2)),border=FALSE,cex=1.3, col="aquamarine4")
boxed.labels(x=2.52,y=0.83, xpad=0.5, ypad=0.2,expression(hat(theta)^(3)),border=FALSE,cex=1.3 ,col="mediumblue")

# unbiasedness

x <- seq(-2,4.,length=1000)
hx <- dnorm(x, mean=1,sd=0.3)
#hv <- dnorm(x, mean=0, sd=0.2)
plot(x, hx, type="l", xlab=" ", cex.lab=1.5, cex.axis=1.3, ylab=" ", main="", lwd=3)
#plot(x, hv, type="l", xlab=(expression(hat(theta))), cex.lab=1.5, cex.axis=1.5,
#  ylab="", main="", lwd=3,yaxt='n')  
lines(x,dnorm(x,mean=1,sd=1), lwd=3, col="mediumblue")
lines(x,dnorm(x,mean=1.8,sd=0.3), lwd=3, col="aquamarine4")

ablineclip(v=1, y2=0.12,untf = FALSE, lty=3, lwd=3)
ablineclip(v=1, y1=0.25,y2=dnorm(1,mean=1,sd=0.3),untf = FALSE, lty=3, lwd=3)

boxed.labels(x=1,y=0.18, ypad=0.9,expression(vartheta),border=FALSE,cex=1.3)

boxed.labels(x=1.4,y=1.2, xpad=0.5, ypad=0.9,expression(hat(theta)^(1)),border=FALSE,cex=1.3)
boxed.labels(x=0.08,y=0.382, xpad=0.5, ypad=0.9,expression(hat(theta)^(3)),border=FALSE,cex=1.3,col="mediumblue" )
boxed.labels(x=2.2,y=1.2, xpad=0.5, ypad=0.2,expression(hat(theta)^(2)),border=FALSE,cex=1.3 , col="aquamarine4")

# Konsistenz


x <- seq(-2,4.,length=1000)
hx <- dnorm(x, mean=1,sd=0.18)
#hv <- dnorm(x, mean=0, sd=0.2)
plot(x, hx, type="l", xlab=" ",ylim=c(0,2.15), yaxt="n", cex.lab=1.5, cex.axis=1.3, ylab=" ", main="", lwd=3)
#plot(x, hv, type="l", xlab=(expression(hat(theta))), cex.lab=1.5, cex.axis=1.5,
#  ylab="", main="", lwd=3,yaxt='n')  
lines(x,dnorm(x,mean=0,sd=1), lwd=3, col="thistle4")
lines(x,dnorm(x,mean=0.5,sd=0.7), lwd=3, col="violetred4")
lines(x,dnorm(x,mean=1,sd=0.5), lwd=3, col="aquamarine4")
lines(x,dnorm(x,mean=1,sd=0.3), lwd=3, col="mediumblue")

ablineclip(v=1, y2=0.05,untf = FALSE, lty=3, lwd=3)
ablineclip(v=1, y1=0.15,untf = FALSE, lty=3, lwd=3)

boxed.labels(x=1,y=0.1, ypad=0.9,expression(vartheta),border=FALSE,cex=1.3)

boxed.labels(x=3,y=1.7, xpad=0.5, ypad=0.9,expression(hat(theta)[50]),border=FALSE,cex=1.3, col="thistle4")
boxed.labels(x=3.05,y=1.5, xpad=0.5, ypad=0.9,expression(hat(theta)[100]),border=FALSE,cex=1.3,col="violetred4" )
boxed.labels(x=3.05,y=1.3, xpad=0.5, ypad=0.2,expression(hat(theta)[500]),border=FALSE,cex=1.3 , col="aquamarine4")
boxed.labels(x=3.09,y=1.1, xpad=0.5, ypad=0.9,expression(hat(theta)[1000]),border=FALSE,cex=1.3,col="mediumblue" )
boxed.labels(x=3.13,y=0.9, xpad=0.5, ypad=0.2,expression(hat(theta)[10000]),border=FALSE,cex=1.3 )

# zentrales Schwankungsintervall

x <- seq(-2,4.,length=1000)
hx <- dnorm(x, mean=1,sd=1)

plot(x, hx, type="l", xlab=" ", ylab=" ", cex.lab=1.5, cex.axis=1.3, main="", lwd=3)
  
ablineclip(v=-0.96,untf = FALSE, lty=1, lwd=3)
ablineclip(v=2.96,untf = FALSE, lty=1, lwd=3)
ablineclip(v=1, y2=0.1,untf = FALSE, lty=3, lwd=3)
ablineclip(v=1, y1=0.17,untf = FALSE, lty=3, lwd=3)

ablineclip(v=0.3, y2=0.1,untf = FALSE, lty=2, lwd=2)
ablineclip(v=0.3, y1=0.17,untf = FALSE, lty=2, lwd=2)

boxed.labels(x=-1.25,y=0.008, ypad=0.4,expression(alpha/2),border=FALSE,cex=1.2)
boxed.labels(x=3.25,y=0.008, ypad=0.4,expression(alpha/2),border=FALSE,cex=1.2)
boxed.labels(x=1,y=0.135, ypad=0.9,expression(vartheta),border=FALSE,cex=1.4)
boxed.labels(x=0.3,y=0.135, ypad=0.9,expression(hat(vartheta)),border=FALSE,cex=1.4)

boxed.labels(x=-1,y=0.14,xpad=0.5, ypad=1,expression(vartheta-c['*']*sigma(hat(theta))),border=FALSE,cex=1.4)
boxed.labels(x=3,y=0.14, xpad=0.5, ypad=1,expression(vartheta+c['*']*sigma(hat(theta))),border=FALSE,cex=1.4)

boxed.labels(x=1.9,y=0.34, xpad=0.5, ypad=0.6,expression(hat(theta)),border=FALSE,cex=1.6)

# symmetrisches Konfidenzintervall


x <- seq(-4,5.,length=1000)
hx <- dnorm(x, mean=1,sd=1)

plot(x, hx, type="l", xlab=" ", ylab=" ", cex.lab=1.5, cex.axis=1.3, main="", lty=3,lwd=3)

lines(x,dnorm(x,mean=-1.66,sd=1), lwd=3,col="mediumblue")
lines(x,dnorm(x,mean=2.26,sd=1),lwd=3,col="mediumblue")

##sch?tzwert
ablineclip(v=0.3,untf = FALSE, lty=1, lwd=3) 
boxed.labels(x=0.3,y=0.12, xpad=1,ypad=3,expression(hat(vartheta)),border=FALSE,cex=1.3)

##wahrer parameter
ablineclip(v=1, y2=0.10,untf = FALSE, lwd=3,lty=3) 
ablineclip(v=1, y1=0.13,untf = FALSE, lwd=3,lty=3)
boxed.labels(x=1,y=0.115, ypad=2,expression(vartheta),border=FALSE,cex=1.3)

##untere Grenze
ablineclip(v=-1.66,untf = FALSE, lwd=2,lty=1,col="mediumblue")
boxed.labels(x=-1.66,y=0.12,xpad=0.5, ypad=1,expression(hat(vartheta)-c['*']*sigma(hat(theta))),border=FALSE,cex=1.2)

##obere Grenze
ablineclip(v=2.26,untf = FALSE, lwd=2,lty=1,col="mediumblue")
boxed.labels(x=2.23,y=0.12, xpad=1, ypad=1,expression(hat(vartheta)+c['*']*sigma(hat(theta))),border=FALSE,cex=1.2)

##Funktionenbeschriftung
boxed.labels(x=-1.1,y=0.39, ypad=0.4,expression(V[u]),border=FALSE,cex=1.3,col="mediumblue")
boxed.labels(x=2.9,y=0.39, ypad=0.4,expression(V[o]),border=FALSE,cex=1.3,col="mediumblue")
boxed.labels(x=1.5,y=0.39, ypad=0.9,expression(hat(theta)),border=FALSE,cex=1.3)


# Konfidenzintervall zu verschiedenen alphas und n's

## alpha=0.05, n=1000-->sigma=0.7

x <- seq(-4,5.,length=1000)
hx <- dnorm(x, mean=1,sd=0.7)

plot(x, hx, type="l", xlab=" ", ylab=" ", cex.lab=1.5, cex.axis=1.3, main="", lty=3,lwd=3)

lines(x,dnorm(x,mean=-1.07,sd=0.7), lwd=3,col="mediumblue")
lines(x,dnorm(x,mean=1.67,sd=0.7),lwd=3,col="mediumblue")

##sch?tzwert
ablineclip(v=0.3,untf = FALSE, lty=1, lwd=3) 
boxed.labels(x=0.3,y=0.15, xpad=1,ypad=3,expression(hat(vartheta)),border=FALSE,cex=1.3)

##wahrer parameter
ablineclip(v=1, y2=0.13,untf = FALSE, lwd=3,lty=3) 
ablineclip(v=1, y1=0.16,untf = FALSE, lwd=3,lty=3)
boxed.labels(x=1,y=0.145, ypad=2,expression(vartheta),border=FALSE,cex=1.3)

##untere Grenze
ablineclip(v=-1.07,untf = FALSE, lwd=2,lty=1,col="mediumblue")
boxed.labels(x=-1.07,y=0.15,xpad=0.5, ypad=1,expression(hat(vartheta)-c['*']*sigma(hat(theta))),border=FALSE,cex=1.2)

##obere Grenze
ablineclip(v=1.67,untf = FALSE, lwd=2,lty=1,col="mediumblue")
boxed.labels(x=1.9,y=0.15, xpad=1, ypad=1,expression(hat(vartheta)+c['*']*sigma(hat(theta))),border=FALSE,cex=1.2)

##Funktionenbeschriftung
boxed.labels(x=-0.55,y=0.55, ypad=0.4,expression(V[u]),border=FALSE,cex=1.3,col="mediumblue")
boxed.labels(x=2.1,y=0.55, ypad=0.4,expression(V[o]),border=FALSE,cex=1.3,col="mediumblue")
boxed.labels(x=0.6,y=0.55, ypad=0.9,expression(hat(theta)),border=FALSE,cex=1.3)


## alpha=0.1, n=100

x <- seq(-4,5.,length=1000)
hx <- dnorm(x, mean=1,sd=1)

plot(x, hx, type="l", xlab=" ", ylab=" ", cex.lab=1.5, cex.axis=1.3, main="", lty=3,lwd=3)

lines(x,dnorm(x,mean=-1.34,sd=1), lwd=3,col="mediumblue")
lines(x,dnorm(x,mean=1.94,sd=1),lwd=3,col="mediumblue")

##sch?tzwert
ablineclip(v=0.3,untf = FALSE, lty=1, lwd=3) 
boxed.labels(x=0.3,y=0.15, xpad=1,ypad=3,expression(hat(vartheta)),border=FALSE,cex=1.3)

##wahrer parameter
ablineclip(v=1, y2=0.13,untf = FALSE, lwd=3,lty=3) 
ablineclip(v=1, y1=0.16,untf = FALSE, lwd=3,lty=3)
boxed.labels(x=1,y=0.145, ypad=2,expression(vartheta),border=FALSE,cex=1.3)

##untere Grenze
ablineclip(v=-1.34,untf = FALSE, lwd=2,lty=1,col="mediumblue")
boxed.labels(x=-1.34,y=0.15,xpad=0.5, ypad=1,expression(hat(vartheta)-c['*']*sigma(hat(theta))),border=FALSE,cex=1.2)

##obere Grenze
ablineclip(v=1.94,untf = FALSE, lwd=2,lty=1,col="mediumblue")
boxed.labels(x=1.94,y=0.15, xpad=1, ypad=1,expression(hat(vartheta)+c['*']*sigma(hat(theta))),border=FALSE,cex=1.2)

##Funktionenbeschriftung
boxed.labels(x=-0.8,y=0.39, ypad=0.4,expression(V[u]),border=FALSE,cex=1.3,col="mediumblue")
boxed.labels(x=2.5,y=0.39, ypad=0.4,expression(V[o]),border=FALSE,cex=1.3,col="mediumblue")
boxed.labels(x=1.5,y=0.39, ypad=0.9,expression(hat(theta)),border=FALSE,cex=1.3)

## alpha 0.05, n=1000

x <- seq(-2,4.,length=1000)
hx <- dnorm(x, mean=1,sd=1)

plot(x, hx, type="l", xlab=" ", ylab=" ", cex.lab=1.5, cex.axis=1.3, main="", lwd=3)
  
ablineclip(v=-0.64,untf = FALSE, lty=1, lwd=3)
ablineclip(v=2.64,untf = FALSE, lty=1, lwd=3)

boxed.labels(x=-1.23,y=0.008, ypad=0.4,expression(alpha/2),border=FALSE,cex=1)
boxed.labels(x=3.23,y=0.008, ypad=0.4,expression(alpha/2),border=FALSE,cex=1)
#boxed.labels(x=1,y=0.135, ypad=0.9,expression(hat(theta)),border=FALSE,cex=1.3)

boxed.labels(x=-0.7,y=0.18,xpad=0.7, ypad=0.6,expression(V[u]),border=FALSE,cex=1.2)
boxed.labels(x=2.7,y=0.18, xpad=0.7, ypad=0.6,expression(V[o]),border=FALSE,cex=1.2)


## alpha 0.1, n=1000

x <- seq(-4,5.,length=1000)
hx <- dnorm(x, mean=1,sd=0.7)

plot(x, hx, type="l", xlab=" ", ylab=" ", cex.lab=1.5, cex.axis=1.3, main="", lty=3,lwd=3)

lines(x,dnorm(x,mean=-0.848,sd=0.7), lwd=3,col="mediumblue")
lines(x,dnorm(x,mean=1.448,sd=0.7),lwd=3,col="mediumblue")

##sch?tzwert
ablineclip(v=0.3,untf = FALSE, lty=1, lwd=3) 
boxed.labels(x=0.3,y=0.05, xpad=1,ypad=3,expression(hat(vartheta)),border=FALSE,cex=1.3)

##wahrer parameter
ablineclip(v=1, y2=0.03,untf = FALSE, lwd=3,lty=3) 
ablineclip(v=1, y1=0.07,untf = FALSE, lwd=3,lty=3)
boxed.labels(x=1,y=0.045, ypad=2,expression(vartheta),border=FALSE,cex=1.3)

##untere Grenze
ablineclip(v=-0.848,untf = FALSE, lwd=2,lty=1,col="mediumblue")
boxed.labels(x=-0.848,y=0.05,xpad=1, ypad=1,expression(hat(vartheta)-c['*']*sigma(hat(theta))),border=FALSE,cex=1.2)

##obere Grenze
ablineclip(v=1.448,untf = FALSE, lwd=2,lty=1,col="mediumblue")
boxed.labels(x=1.75,y=0.05, xpad=1, ypad=1,expression(hat(vartheta)+c['*']*sigma(hat(theta))),border=FALSE,cex=1.2)

##Funktionenbeschriftung
boxed.labels(x=-0.4,y=0.55, ypad=0.4,expression(V[u]),border=FALSE,cex=1.3,col="mediumblue")
boxed.labels(x=2.1,y=0.55, ypad=0.4,expression(V[o]),border=FALSE,cex=1.3,col="mediumblue")
boxed.labels(x=0.6,y=0.55, ypad=0.9,expression(hat(theta)),border=FALSE,cex=1.3)


# Kleinstquadrat-Schätzer 3D

b0<-0.2

set.seed(2)
e<-rnorm(100, mean=0, sd=1.5)
y<-seq(8,-8,length=100)
x<-b0+e

x1<-rep(0,100)
x2<-rep(-5,100)
x3<-rep(5,100)

z=dnorm(y,mean=0.2,sd=1.5)
#z2=dnorm(y,mean=1,sd=1.5)
#z3=dnorm(y,mean=1,sd=1.5)

plot<-scatterplot3d(x2,y,z,type="l",grid=TRUE, box=TRUE,angle=60, xlab="Tag", ylab="Fahrzeit",cex.lab=1.3,axis=TRUE, 
                    tick.marks=FALSE, ylim=c(-10,10),xlim=c(-10,10), zlab=" ",xaxt=NULL,lwd=3, 
                    cex.axis=1.5)

z0<-rep(0,100)
x0<-seq(10,-10,length=100)
y0<-b0+e
plot$points3d(x0,y0,z0, pch=20, col="grey", lwd=0.5) #points
y0<-b0+0*x
plot$points3d(x0,y0,z0, type="l", lwd=3, lty="dashed") #true regression line

#curves

plot$points3d(x1,y,z, type="l", lwd=3) 
plot$points3d(x3,y,z, type="l", lwd=3)

#means

y5<-rep(0.2,100)
x51<-rep(5,100)
x52<-rep(0,100)
x53<-rep(-5,100)
z5<-seq(0,dnorm(0,mean=0,sd=1.5),length=100)
plot$points3d(x51,y5,z5, type="l", lwd=2) 
plot$points3d(x52,y5,z5, type="l", lwd=2)
plot$points3d(x53,y5,z5, type="l", lwd=2)

# Kleinstquadrat-Schätzer 2D
set.seed(2)
e<-rnorm(100, mean=0, sd=1.5)
set.seed(2)
x<-seq(10,-10,length=100)
b0=0.2
#b1=0.8
y<-b0+e
plot(x,y, xlim=c(-10,10), ylim=c(-10,10), pch=16,ann=FALSE, 
      col="grey30", xaxt="n", yaxt="n")
mtext(side = 1, text = "Tag", line = 1, cex=1.3)
mtext(side = 2, text = "Fahrzeit", line = 1,cex=1.3)
ablineclip(h=0.2, x2=-9, lty="dashed", lwd=3)
ablineclip(h=0.2, x1=-8, lty="dashed", lwd=3)
boxed.labels(x=-8.5,y=0.2, expression(vartheta),cex=2, xpad=2.5, ypad=3, border=FALSE)

#lines(x,y=-0.1+0*x, lty="dashed", lwd=3, col="blue") # geschätzer Parameter
ablineclip(h=-0.5, x2=-8, lty="dashed", lwd=3, col="blue")
ablineclip(h=-0.5, x1=-7, lty="dashed", lwd=3, col="blue")
boxed.labels(x=-7.5,y=-0.5, expression(hat(vartheta)),cex=2, xpad=1.8, ypad=2, border=FALSE)


