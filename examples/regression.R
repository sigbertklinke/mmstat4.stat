x <- c(0,5)
y <- x+1
xi <- 3
yh <- xi+1
yi <- yh+1
pdf("regression.pdf", width=6, height=4)
plot(x,y, xlim=c(0,5), ylim=c(-1,6), type="l", axes=F, col="red")
axis(1, at=c(0,1,xi), labels=c("0", "1", expression(x[i])))
axis(2, at=c(0,yi,yh), labels=c("0", expression(y[i]), expression(hat(y)[i])), las=2)
box()
#
points(xi,yi,pch=19)
points(xi,yh,pch=19, col="red")
#
text(-0.25, yh-0.5, "Regresswert", col="red", pos=4)
lines(c(-1,xi), c(yh,yh), col="red", lty="dotted")
lines(c(xi-0.01, xi-0.01), c(-2,yh), col="red", lty="dotted")
lines(c(-1,xi), c(yi,yi),lty="dotted")
lines(c(xi+0.01,xi+0.01), c(-2,yi), lty="dotted")
text(xi,yi, "Beobachtung", pos=3)
#
lines(c(0,0), c(0,1), lty="dotted", col="blue")
text(0, 0.25, expression(b[0]), col="blue", pos=4)
lines(c(0,1,1), c(1,1,2), lty="solid", col="blue")
text(1, 1.25, expression(b[1]), col="blue", pos=4)
dev.off()