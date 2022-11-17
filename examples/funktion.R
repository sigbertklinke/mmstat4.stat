x <- (-200:200)/100

y1 <- x+0.5
y2 <- x^2-1
y3 <- x^3/3

ylim <- range(y1, y2, y3)
pdf("funktion.pdf", width=10, height=7)
plot(x, y1, ylim=ylim, type="l", col="red", xlab="X: Definitionsbereich", ylab="Y: Wertebereich")
lines(x, y2, col="blue")
lines(x, y3, col="green")
legend("bottomright", legend=c("Lineare Funktion", "Quadratische Funktion", "Kubische Funktion"),
			 col=c("red", "blue", "green"), lwd=2)
dev.off()