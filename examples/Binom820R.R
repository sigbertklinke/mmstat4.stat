x <- seq(0,19,by=1)
y <- dbinom(x,19,0.35)
names(y) <- sprintf("%.0f", x)
col <- rep("gray", 19)
col[14:20] <- "red"

pdf("Binom820R.pdf")
par(bg="transparent")
barplot(y, xlab="x", ylab="P(X=x)", main="X~B(n=19; p=0.35)", col=col)
dev.off()