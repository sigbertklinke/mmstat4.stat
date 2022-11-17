N <- 1000
M <- 400
n <- 15
x <- 0:n
(n/N<0.05)
freq <- dhyper(x, m=M, n=N-M, k=n)
plot(x, freq, type="h",
		 main="Hypergeo. Verteilung -> Binomialverteilung")
points(x, freq, pch=19)
bin <- dbinom(x, size=n, prob=M/N)
lines(x+0.1, bin, type="h", col="red")
points(x+0.1, bin, pch=19, col="red")