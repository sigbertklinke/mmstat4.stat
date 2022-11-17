n <- c(500, 5000, 50000)
b <- c(35, 75, 250)
lims <- c(0.437,2.236)
x <- rnorm(max(n))
pdf("pic_disc_cont.pdf")
par(mfrow=c(4,1), mar=c(0, 5, 3,1))
for (i in seq(n)) {
  xi <- sample(x, n[i])
  h  <- hist(xi, breaks=b[i], plot=F)
  sf <- stepfun(h$breaks, c(0, h$density, 0))
  plot(sf, do.points = F, xlim=c(-4,4), ylim=c(0, 0.5),
       main=sprintf("n=%i", n[i]), ylab="Hfk.dichte", xlab="", 
       axes=F, cex.main=2, cex.axis=2, cex.lab=2)
  axis(2, ce.xaxis=2)
  box()
  index <- seq(h$breaks)[(h$breaks>lims[1]) & (h$breaks<lims[2])]
  px    <- c(lims[1], lims[1], h$breaks[index[1]])
  py    <- c(0, sf(lims[1]), sf(lims[1]))
  for (j in index) {
    px <- c(px, h$breaks[j], h$breaks[j+1])
    py <- c(py, sf(h$breaks[j]), sf(h$breaks[j]))
  }
  px    <- c(px, lims[2], lims[2])
  py    <- c(py, sf(lims[2]), 0)
  polygon(px, py, col="green")
}
x <- seq(-4, 4, length.out=501)
plot(x, dnorm(x), main="Modell", ylab="Dichte", type="l", 
        xlab="", ylim=c(0, 0.5), cex.main=2,  cex.lab=2)
xi <- x[(x>lims[1]) & (x<lims[2])]
px <- c(lims[1], lims[1], xi, lims[2], lims[2])
py <- c(0, dnorm(lims[1]), dnorm(xi), dnorm(lims[2]), 0)
polygon(px, py, col="green")
dev.off()

