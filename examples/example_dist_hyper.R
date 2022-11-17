library("mmstat4")
n <- 10
N <- 15
M <- 5
x <- 0:n
# Zufallsvariable
par(mfrow=c(2,2))
plot(x, dhyper(x, M, N-M, n), type="h", xlab="x",
     ylab="Wahrscheinlichkeit",
     main="Wahrscheinlichkeitsfunktion f(x)")
df <- cdf(x, phyper(x, M, N-M, n))
plot(df, xlab="x", ylab="Kum. Wahrscheinlichkeit",
     main="Verteilungsfunktion F(x)")
# Stichprobe
xs  <- rhyper(20, M, N-M, n)
tab <- table(factor(xs, levels=0:n))
barplot(tab, main="Haeufigkeit h(x)", xlab="x", ylab="Anzahl",
        sub=sprintf("n=%.0f", n))
stripchart(xs, method="jitter", ylim=c(0.75,1.25), pch=19,
           main="Dotplot", xlim=range(x))
