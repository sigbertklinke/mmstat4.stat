library("mmstat4")
n <- 10
p <- 0.2
x <- 0:n
# Zufallsvariable
par(mfrow=c(2,2))
plot(x, dbinom(x, size=n, prob=p), type="h", xlab="x",
     ylab="Wahrscheinlichkeit",
     main="Wahrscheinlichkeitsfunktion f(x)")
df <- cdf(x, pbinom(x, size=n, prob=p))
plot(df, xlab="x", ylab="Kum. Wahrscheinlichkeit",
     main="Verteilungsfunktion F(x)")
# Stichprobe
xs  <- rbinom(20, size=n, prob=p)
tab <- table(factor(xs, levels=0:n))
barplot(tab, main="Häufigkeit h(x)", xlab="x", ylab="Anzahl",
        sub=sprintf("n=%.0f", n))
stripchart(xs, method="jitter", ylim=c(0.75,1.25), pch=19,
           main="Dotplot", xlim=range(x))
