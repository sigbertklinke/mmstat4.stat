############################################################################
################## Grafiken Zeitreihentheorie ##################################
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

#setwd("C:/Users/Franzi/Desktop/Franzi/Arbeit/Zeitreihen")

## linear Trend
x <- seq(0,10,by=1)
y<-x
par(bg="transparent")
plot(x,y, type="l", xlab=" ", ylab=" ", xaxt="n", yaxt="n")

## exponentialtrend
x <- seq(0,10,by=0.1)
y<-x^3
par(bg="transparent")
plot(x,y, type="l", xlab=" ", ylab=" ", xaxt="n", yaxt="n")

### Telefon

TelefonDaten <- read.table("TelefonDaten.csv", sep=";", quote="\"")
TelefonDaten$V3 <- TelefonDaten$V2/1000000
TelefonDaten <- TelefonDaten[25:95,]

par(bg="transparent")
plot(TelefonDaten$V1,TelefonDaten$V3, type="l", xlim=c(1900,1970), xlab="Jahr", ylab="Anzahl der Telefone in USA (in Mio.)", lwd=2)
lines(TelefonDaten$V1,TelefonDaten$V3, type="p", lwd=2, pch=20)
# Exponential Trend

TelefonDaten$V4<-log(TelefonDaten$V3)
#temp <- TelefonDaten[25:95,]

t<-seq(1,71,by=1)
lm(TelefonDaten$V4~t)

fit<-exp(8.18407)*exp(0.04937)^t

summary.lm(lm(TelefonDaten$V4~t))
fit.ts<-ts(fit,start=1900,frequency=1)

par(bg="transparent")
plot(TelefonDaten$V1,TelefonDaten$V3, type="p",pch=20, xlim=c(1900,1970), xlab="Jahr", ylab="Anzahl der Telefone in USA (in Mio.)", lwd=2)#, xlim=c(2008,2012), ylim=c(200000,260000))
lines(TelefonDaten$V1,fit/1000,type="l", col="red",lwd=2)

### Arbeitslose

ArbeitsloseBerlin <- ts(read.table("ArbeitsloseBerlin.csv", quote="\""),start=2005,frequency=12)

par(bg="transparent")
plot(ArbeitsloseBerlin/1000, type="l", xlab="Jahr", ylab="Anzahl Arbeitsloser in Berlin (in 1000)", lwd=2)#, xlim=c(2008,2012), ylim=c(200000,260000))
lines(ArbeitsloseBerlin/1000, type="p", lwd=2, pch=20)
# gleitender durchschnitt

ArbeitsloseBerlin.ma4<-filter(ArbeitsloseBerlin/1000, rep(1/3, 3), method = "convolution", sides = 2)
ArbeitsloseBerlin.ma6<-filter(ArbeitsloseBerlin/1000, rep(1/6, 6), method = "convolution", sides = 2)
ArbeitsloseBerlin.ma12<-filter(ArbeitsloseBerlin/1000, rep(1/12, 12), method = "convolution", sides = 2)

plot(ArbeitsloseBerlin/1000, type="p", xlab="Jahr", ylab="Anzahl Arbeitsloser in Berlin (in 1000)", lwd=2, pch=20)#, xlim=c(2008,2012), ylim=c(200000,260000))
lines(ArbeitsloseBerlin.ma6, type="l", col="springgreen4", lwd=2)
lines(ArbeitsloseBerlin.ma4, type="l", col="mediumblue", lwd=2)
lines(ArbeitsloseBerlin.ma12, type="l", col="red", lwd=2)

boxed.labels(x=2011,y=320, xpad=0.5, ypad=0.9,expression(k==3),border=FALSE,cex=1.2,bg="transparent",col="mediumblue")
boxed.labels(x=2011,y=310, xpad=0.5, ypad=0.9,expression(k==6),border=FALSE,cex=1.2,bg="transparent",col="springgreen4")
boxed.labels(x=2011.05,y=300, xpad=0.5, ypad=0.9,expression(k==12),border=FALSE,cex=1.2,bg="transparent",col="red")
# decompose

decompose(ArbeitsloseBerlin/1000, type ="additive" ,filter = NULL)
temp1<-decompose(ArbeitsloseBerlin/1000, type ="additive" ,filter = NULL)$trend
temp1a<-decompose(ArbeitsloseBerlin/1000, type ="additive" ,filter = NULL)$figure
temp2<-decompose(ArbeitsloseBerlin/1000, type ="additive" ,filter = NULL)$trend+decompose(ArbeitsloseBerlin/1000, type ="additive" ,filter = NULL)$seasonal
par(bg="transparent")
plot(ArbeitsloseBerlin/1000, type="p", pch=20, xlab="Jahr", ylab="Anzahl Arbeitsloser in Berlin (in 1000)", lwd=2)#, xlim=c(2008,2012), ylim=c(200000,260000))
lines(temp1, type="l", col="red", lwd=2)
lines(temp2, type="l", col="springgreen4", lwd=2)

boxed.labels(x=2009,y=320, xpad=0.5, ypad=0.9,expression(Trend),border=FALSE,cex=1.2,bg="transparent",col="red")
boxed.labels(x=2010.2,y=310, xpad=0.5, ypad=0.9,expression(Trend*"+"*periodische*" "*Schwankungen),border=FALSE,cex=1.2,bg="transparent",col="springgreen4")

# G?te

random=decompose(ArbeitsloseBerlin, type ="additive" ,filter = NULL)$random
random<-random[7:76]
szrm=sqrt(mean(random^2))

xbar=mean(ArbeitsloseBerlin)
sx=sqrt(mean((ArbeitsloseBerlin-xbar)^2))
R2=1-szrm/xbar
### Preisindex

Preisindex <- ts(read.table("Preisindex.csv", dec=",", quote="\""),start=2005,frequency=12)

par(bg="transparent")
plot(Preisindex, type="l", xlab="Jahr", ylab="Preisindex f?r Nettokaltmieten in Berlin (Juni 2005=100)", lwd=2)#, xlim=c(2008,2012), ylim=c(200000,260000))
lines(Preisindex, type="p", lwd=2, pch=20)

# linearer Trend
#t<-seq(2005,2011.833,length=length(Preisindex))
t<-seq(1,82,by=1)
lm(Preisindex~t)
fit<-lm(Preisindex~t)$fit
summary.lm(lm(Preisindex~t))
fit.ts<-ts(fit,start=2005,frequency=12)

par(bg="transparent")
plot(Preisindex, type="p", pch=20,xlab="Jahr", ylab="Preisindex f?r Nettokaltmieten in Berlin (Juni 2005=100)", lwd=2)#, xlim=c(2008,2012), ylim=c(200000,260000))
lines(fit.ts,type="l", col="red",lwd=2)


### Variable 
set.seed(3)
x<-ts(rnorm(44, mean = 10, sd = 0.5),start=2000,frequency=4)

par(bg="transparent")
plot(x, type="l",lwd=2, ylab="Merkmal X", xlab="Zeit")

