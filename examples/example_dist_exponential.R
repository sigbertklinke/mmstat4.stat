l <- 3
x <- seq(0, 3*l, by=0.1)
dens <- dexp(x, rate=l)
plot(x, dens, type="l", 
		 main="Dichtefunktion Exponentialverteilung")
cdf  <- pexp(x, rate=l)
plot(x, cdf, type="l", 
		 main="Verteilungsfunktion Exponentialverteilung")