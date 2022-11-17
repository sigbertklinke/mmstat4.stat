xlim  <- c(-3, 3)
ylim  <- c(0, 3)
a <- 0
b <- 1
x <- seq(xlim[1], xlim[2], by=0.1)
dens <- stepfun(c(a,b), c(0, 1/(b-a), 0))
plot(dens, xlim=xlim, ylim=ylim, verticals=F, pch=NA,
		 main="Dichtefunktion Gleichverteilung")
cdf  <- punif(x, min=a, max=b)
plot(x, cdf, type="l", 
		 main="Verteilungsfunktion Gleichverteilung")