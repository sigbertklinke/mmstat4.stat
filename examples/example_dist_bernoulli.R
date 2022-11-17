p <- 0.7
n <- 1
x <- 0:n
freq <- dbinom(x, size=n, prob=p)
plot(x, freq, type="h", main="Wahrscheinlichkeitsfunktion Bernoulliverteilung",
		 ylim=c(0,1))
points(x, freq, pch=19)
cdf <- approxfun(x, cumsum(freq), method="constant",
								 yleft=0, yright=1, f=0)
class(cdf) <- c("ecdf", "stepfun", class(cdf))
plot(cdf, main="Verteilungsfunktion Bernoulliverteilung"")