df   <- 50
x    <- seq(-3, 3, by=0.1)
dens <- dt(x, df)
plot(x, dens, type="l", ylim=c(0, 0.4),
		 main="Dichtefunktion t-Verteilung")
cdf  <- pt(x, df)
plot(x, cdf, type="l", 
		 main="Verteilungsfunktion  t-Verteilung")