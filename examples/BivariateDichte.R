plot.copula2 <- function (copula, x, y, add.margins=F) {
  xy   <- as.matrix(expand.grid(x,y))
  z    <- matrix(dmvdc(copula, xy), nr=length(x),nc=length(y))
  zmax <- max(z)
  if (add.margins) {
    margins <- slot(copula, "margins")
    params  <- slot(copula, "paramMargins")

    paraml  <- c(x=x, params[[1]])
    dx      <- do.call(match.fun(paste("d", margins[1], sep="")), paraml)
    paraml  <- c(x=y, params[[2]])
    dx      <- do.call(match.fun(paste("d", margins[2], sep="")), paraml)
  }
  pmat <- persp(x, y, z, zlim=c(0, zmax), axes=F, theta=-30, phi=45)
  if (add.margins) {
    lines(trans3d(x,min(y),dx, pmat), col="red")
    lines(trans3d(min(x),y,dy, pmat), col="red")
  }
  box()
  contour(x, y, z, nlevels=10, axes=F)
  box()
}


library("copula")
gaussMVD  <- mvdc(normalCopula(0),margins=c("norm","norm"),paramMargins=list(list(mean=0,sd=1),list(mean=0, sd=1)))
plot.copula2(gaussMVD, u, u)
plot.copula2(gaussMVD, u, u, T)

pdf("BivariateDichte1.pdf", width=10,height=5)

par(mfcol=c(2,4), mar=c(0,0,0,0))


gaussMVD2 <- mvdc(normalCopula(0.75),margins=c("norm","norm"),paramMargins=list(list(mean=0,sd=1),list(mean=0, sd=1)))
clayMVD   <- mvdc(claytonCopula(0.5),margins=c("norm","norm"),paramMargins=list(list(mean=0,sd=1),list(mean=0, sd=1)))
n <- 20
u <- (-n:+n)/(n/3)
z <- matrix(0, nc=length(u), nr=length(u))
z[abs(u)<1,abs(u)<1] <- 1/4
u2    <- as.matrix(expand.grid(u,u))
zMVD  <- matrix(dmvdc(gaussMVD, u2), nr=length(u),nc=length(u))
res<-persp(x=u, y=u, z=zMVD, axes=F, theta=-30, phi=45)


persp(gaussMVD, dmvdc, xlim = c(-3, 3), ylim = c(-3, 3),xlab="",ylab="", cex.axis = 1.5, axes=F, theta=-30, phi=45)
box()
contour(gaussMVD, dmvdc, xlim = c(-3, 3), ylim = c(-3, 3),nlevels=10,xlab="",ylab="", cex.axis = 1.5, axes=F)
box()

persp(gaussMVD2, dmvdc, xlim = c(-3, 3), ylim = c(-3, 3),xlab="",ylab="", cex.axis = 1.5, axes=F, theta=-30, phi=45)
box()
contour(gaussMVD2, dmvdc, xlim = c(-3, 3), ylim = c(-3, 3),nlevels=10,xlab="",ylab="", cex.axis = 1.5, axes=F)
box()

persp(clayMVD, dmvdc, xlim = c(-3, 3), ylim = c(-3, 3),xlab="",ylab="", cex.axis = 1.5, axes=F, theta=-30, phi=45)
box()
contour(clayMVD, dmvdc, xlim = c(-3, 3), ylim = c(-3, 3),nlevels=10,xlab="",ylab="", cex.axis = 1.5, axes=F)
box()

persp(x=u,y=u,z=z,xlab="",ylab="", cex.axis = 1.5, axes=F, theta=-30, phi=45)
#contour(x=u,y=u,z=z,xlab="",ylab="", cex.axis = 1.5, axes=F, theta=-30, phi=45)
box()

dev.off()


pdf("BivariateDichte2.pdf", width=10,height=5)
par(mfcol=c(2,4), mar=c(0,0,0,0))

dmvdc(gaussMVD, 

res <- persp(gaussMVD, dmvdc, xlim = c(-3, 3), ylim = c(-3, 3),xlab="",ylab="", cex.axis = 1.5, axes=F, theta=-30, phi=45)
lines(trans3d(x=u, y=-3, z=dnorm(u), pmat=res), col="red")
lines(trans3d(x=-3, y=u, z=dnorm(u), pmat=res), col="red")
box()
contour(gaussMVD, dmvdc, xlim = c(-3, 3), ylim = c(-3, 3),nlevels=10,xlab="",ylab="", cex.axis = 1.5, axes=F)
box()

persp(gaussMVD2, dmvdc, xlim = c(-3, 3), ylim = c(-3, 3),xlab="",ylab="", cex.axis = 1.5, axes=F, theta=-30, phi=45)
box()
contour(gaussMVD2, dmvdc, xlim = c(-3, 3), ylim = c(-3, 3),nlevels=10,xlab="",ylab="", cex.axis = 1.5, axes=F)
box()

persp(clayMVD, dmvdc, xlim = c(-3, 3), ylim = c(-3, 3),xlab="",ylab="", cex.axis = 1.5, axes=F, theta=-30, phi=45)
box()
contour(clayMVD, dmvdc, xlim = c(-3, 3), ylim = c(-3, 3),nlevels=10,xlab="",ylab="", cex.axis = 1.5, axes=F)
box()

res <- persp(x=u,y=u,z=z,xlab="",ylab="", cex.axis = 1.5, axes=F, theta=-30, phi=45)
#contour(x=u,y=u,z=z,xlab="",ylab="", cex.axis = 1.5, axes=F, theta=-30, phi=45)
box()

dev.off()
