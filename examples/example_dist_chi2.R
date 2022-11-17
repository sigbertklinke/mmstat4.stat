df   <- 3
x    <- seq(0, 3*df, by=0.1)
dens <- dchisq(x, df)
plot(x, dens, type="l", ylim=c(0, 0.4),
		 main="Dichtefunktion Chi²-Verteilung")
cdf  <- pchisq(x, df)
plot(x, cdf, type="l", 
		 main="Verteilungsfunktion  Chi²-Verteilung")