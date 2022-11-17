m <- 6 # Würfel ;)
par(mfrow=c(2,2))
plot(1:m, rep(1/m, m), type="h", xlab="x", ylab="Wahrscheinlichkeit",
     main="Wahrscheinlichkeitsfunktion f(x)")
plot(ecdf(1:m), xlab="x", ylab="Kum. Wahrscheinlichkeit",
     main="Verteilungsfunktion F(x)")
# Stichprobe
n <- 20
x   <- sample(1:m, size=n, replace=TRUE)
tab <- table(factor(x, levels=1:m))
barplot(table(x), main="Häufigkeit h(x)", xlab="x", ylab="Anzahl",
        sub=sprintf("n=%.0f", n))
stripchart(x, method="jitter", ylim=c(0.75,1.25), pch=19,
           main="Dotplot")
