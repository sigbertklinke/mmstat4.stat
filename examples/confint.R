pdf("confint.pdf", width=10, height=5)
tr <- range(c(qnorm(c(0.0001, 0.9999)), qnorm(c(0.0001, 0.9999), -1.5)))
t  <- min(tr)+diff(tr)*(0:200)/200
yt <- dnorm(t)
th <- -1
c  <- 1.96
par(mar=c(3.5,0,0,0))
plot(t, yt, type="l", axes=F, xlab="", ylab="", ylim=c(-0.04, max(yt)))
axis(1, at=c(0, -c, c, th, th-c, th+c), 
     labels=c(expression(theta), expression(c[u]), expression(c[o]), '', '', ''),
  #            expression(hat(theta)), expression(hat(theta)+c[u]),  expression(hat(theta)+c[o])), 
     cex.axis=1.5)
usr <- par('usr')
mtext(c(expression(hat(theta)), expression(hat(theta)+c[u]),  expression(hat(theta)+c[o])), 
      side=1, at=c(th, th-c, th+c), cex=1.5, line=2.5)
#, (usr[3])*c(1,1,1))
box()

tp <- t[(t>-c)&(t<c)]
tp <- c(-c, tp, c)
yp <- dnorm(tp)
tp <- c(-c, tp, c)
yp <- c(0, yp, 0)
polygon(tp, yp, border="black", col="#66C2A5")
lines(c(th,th), c(0, usr[4]), lty="dashed")
lines(c(0,0), c(0, max(yt)))
polygon(c(th-c, th-c, th+c, th+c), c(usr[1],0,0,usr[1]), border="black", col="#FC8D62")
lines(t, dnorm(t, th-c), col="gray")
lines(t, dnorm(t, th+c), col="gray")

arrows(0, 0.03, -c, 0.03)
arrows(0, 0.03, c, 0.03)
arrows(th, -0.04, th-c, -0.04)
arrows(th, -0.04, th+c, -0.04)
lines(c(th-c, th-c), c(0, usr[1]))
lines(c(th+c, th+c), c(0, usr[1]))
text(th, -0.005, expression(paste(1-alpha, " Konfidenzintervall")), pos=1, cex=1.5)
text(0, 0.03, expression(paste(1-alpha, " Schwankungsintervall")), pos=3, cex=1.5)

text(th-c-0.75, dnorm(th-c-0.75, th-c), expression(paste(hat(Theta), '+', c[u])), pos=2, col="gray", cex=1.5)
text(th+c+0.75, dnorm(th+c+0.75, th+c), expression(paste(hat(Theta), '+', c[o])), pos=4, col="gray", cex=1.5)
abline(h=0)
text(0.25, dnorm(0.25), expression(paste(hat(Theta), ' ~ ', N(theta, sigma[hat(Theta)]^2))), pos=4, cex=1.5)
text(usr[2], dnorm(0.25), expression(paste(sigma[hat(Theta)], ' = Standardfehler')), pos=2, cex=1.25)
dev.off()