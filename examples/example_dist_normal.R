xlim  <- c(-5, 5)
ylim  <- c(0, 1)
mu    <- 0
sigma <- 1
x     <- seq(xlim[1], xlim[2], by=0.1)
dens  <- dnorm(x, mean=mu, sd=sigma)
plot(x, dens, type="l", xlim=xlim, ylim=ylim,
		 main="Dichtefunktion Normalverteilung")
#
mu    <- 0
sigma <-1 
cdf  <- pnorm(x, mean=mu, sd=sigma)
plot(x, cdf, type="l", xlim=xlim, ylim=c(0,1),
		 main="Verteilungsfunktion Normalverteilung")