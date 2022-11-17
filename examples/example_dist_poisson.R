l <- 5
x <- 0:(3*l)
freq <- dpois(x, lambda=l)
plot(x, freq, type="h", main="Wahrscheinlichkeitsfunktion Poissonverteilung")
points(x, freq, pch=19)
cdf <- approxfun(x, cumsum(freq), method="constant",
								 yleft=0, yright=1, f=0)
class(cdf) <- c("ecdf", "stepfun", class(cdf))
plot(cdf, main="Verteilungsfunktion Poissonverteilung")