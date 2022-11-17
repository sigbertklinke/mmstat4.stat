df1 <- 5
df2 <- 10
x <- seq(0, 3*l, by=0.1)
dens <- df(x, df1=df1, df2=2)
plot(x, dens, type="l", 
		 main="Dichtefunktion F-Verteilung")
cdf  <- pf(x, df1=df1, df2=2)
plot(x, cdf, type="l", 
		 main="Verteilungsfunktion F-Verteilung")