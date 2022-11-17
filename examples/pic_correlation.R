n <- 1000
x <- rnorm(n)
cex <- 0.25
cex.main <- 1.75
pdf("pic_correlation.pdf", width=10, height=5)
par(mar=c(0,0,1,0), mfrow=c(3,7))
# First row
wseq <- seq(-1, 1, by=1/3)
for (w in wseq) {
  y <- (1-abs(w))*rnorm(n)
  r <- -sign(w)*pi/4
  xr <- x*cos(r)-y*sin(r)
  yr <- x*sin(r)+y*cos(r)
  plot(xr, yr, asp=T, pch=19, xlim=c(-5,5), ylim=c(-5,5), cex=cex,
       main=sprintf("%+.1f", cor(xr,yr)), axes=F, cex.main=cex.main)
}
# Second row
for (w in wseq) {
  y <- -w*x
  plot(x, y, asp=T, pch=19, xlim=c(-5,5), ylim=c(-5,5), cex=cex,
       main=sprintf("%+.1f", cor(x,y)), axes=F, cex.main=cex.main)
}
# Third row
x  <- 2*runif(n)-1
#
xr <- x+runif(n)/3
yr <- 4*(x^2 - 1/2)^2 + runif(n)/500
plot(xr, yr, asp=T, pch=19, cex=cex, main="0.0", axes=F, cex.main=cex.main)
#
y <- 2*runif(n)-1
r <- -pi/8
xr <- x*cos(r)-y*sin(r)
yr <- x*sin(r)+y*cos(r)
plot(xr, yr, asp=T, pch=19, cex=cex, main="0.0", axes=F, cex.main=cex.main)
#
r  <- -pi/4
xr <- x*cos(r)-y*sin(r)
yr <- x*sin(r)+y*cos(r)
plot(xr, yr, asp=T, pch=19, cex=cex, main="0.0", axes=F, cex.main=cex.main)
#
xr <- x/2
yr <- (x^2 + runif(n))/2
plot(xr, yr, asp=T, pch=19, cex=cex, main="0.0", axes=F, cex.main=cex.main)
# 
xr <- x
yr <- (x^2 + runif(n)/2)*ifelse(runif(n)>0.5, +1, -1)
plot(xr, yr, asp=T, pch=19, cex=cex, main="0.0", axes=F, cex.main=cex.main)
#
xr <- sin(pi*x)+rnorm(n, sd=1/8)
yr <- cos(pi*x)+rnorm(n, sd=1/8)
plot(xr, yr, asp=T, pch=19, cex=cex, main="0.0", axes=F, cex.main=cex.main)
#
xr <- c(rnorm(n/4), 6+rnorm(n/4), rnorm(n/4), 6+rnorm(n/4))
yr <- c(rnorm(n/4), rnorm(n/4), 6+rnorm(n/4), 6+rnorm(n/4))
plot(xr, yr, asp=T, pch=19, cex=cex, main="0.0", axes=F, cex.main=cex.main)
dev.off()