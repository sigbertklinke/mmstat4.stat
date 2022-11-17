n <- 20
p <- 0.01
(n>10) && (p<0.05)
x    <- 0:n
freq <- dbinom(0:n, n, p)
plot(x, freq, type="h",
		 main="Binomialverteilung -> Poissonverteilung")
points(x, freq, pch=19)
pois <- dpois(x, lambda=n*p)
lines(x+0.1, pois, type="h", col="red")
points(x+0.1, pois, pch=19, col="red")