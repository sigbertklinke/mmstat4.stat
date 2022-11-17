mu <- 3.41
sd <- 0.7
x  <- c(1.95, 2.45, 2.95, 3.45, 3.95, 4.45)
dx <- dnorm(x, mean=mu, sd=sd)

pdf("gluehlampen.pdf", width=10, height=7)
par(mfcol=c(2,2), mar=c(2,2,3,0.5))
x1 <- seq(mu-4*sd, mu+4*sd, length.out = 201)
plot(x1, dnorm(x1, mean=mu, sd=sd), type="l", main="X~N(3,41; 0,7)", xlab="", ylab="", lwd=2)
for (i in seq(x)) lines(c(x[i],x[i]), c(0, dx[i]), col="blue")

x0 <- seq(-4, 4, length.out = 201)
plot(x0, dnorm(x0), type="l", main="Z~N(0; 1)", xlab="", ylab="", lwd=2)
z  <- round((x-mu)/sd, 2)
dz <- dnorm(z)
for (i in seq(x)) lines(c(z[i],z[i]),  c(0, dz[i]), col="blue")

dz <- pnorm(z)
plot(c(0,1), c(0,1), type="n", axes=F)
text(0.375, 0.9, "Klassengrenzen in")
text(0.25, 0.8, "x")
text(0.5,  0.8, "z")
text(0.75, 0.8, expression(Phi(z)))
for (i in seq(x)) {
  text(0.25, 0.8-0.1*i, x[i])
  text(0.5,  0.8-0.1*i, round(z[i],2))  
  text(0.75,  0.8-0.1*i, round(dz[i],6))  
}

plot(x0, pnorm(x0), type="l", main=expression(Phi(z)), xlab="", ylab="", lwd=2)

for (i in seq(x)) {
  lines(c(z[i],z[i]),  c(0, dz[i]), col="blue")
  lines(c(-4.5,z[i]), c(dz[i], dz[i]), col="blue")
  text(2, dz[i], round(dz[i], 6), pos=4, cex=0.8, col="blue")
}
dev.off()


