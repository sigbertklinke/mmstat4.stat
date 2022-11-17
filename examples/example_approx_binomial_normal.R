n <- 100
p <- 0.3
(n*p*(1-p))>9
x    <- 0:n
freq <- dbinom(0:n, n, p)
plot(x, freq, type="h", ylim=c(0,1/sqrt(2*pi*n*p*(1-p))),
		 main="Binomialverteilung -> Normalverteilung")
points(x, freq, pch=19)
xn   <- seq(0, n, by=0.1)
norm <- dnorm(xn, mean=n*p, sd=sqrt(n*p*(1-p)))
lines(xn, norm, col="red", lwd=2)