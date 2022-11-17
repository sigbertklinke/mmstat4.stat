draw.polygon <- function (x, from, to, col, d=0) {
  xp <- seq(from, to, length.out=sum((x<=to) & (x>=from)))
  yp <- dnorm(xp, d)
  polygon(c(from, xp, to), c(0, yp, 0), col=col) 
}

text.blue <- function (x, txt) {
  polygon(c(x-0.4,x-0.4,x+0.4,x+0.4), c(0.05,0.15,0.15,0.05), col="white")
  text(x, 0.1, txt, cex=1.2)
}

text.black <- function (x, txt) {
  text(x, 0.1, txt, cex=1.2)
}

c <- 1.96
x <- (-350:350)/100
y <- dnorm(x)

#
cairo_pdf("test1.pdf")
par(mfrow=c(3,1))

plot(x,y, type="l", axes=F, xlab="v", ylab="f(v)", ylim=c(0,0.5), cex.main=2, cex.lab=1.2,
     main=expression(paste("Zweiseitiger Test:  ", H[0], ": ", theta1==theta1[0], "  vs.  ", H[1], ": ", theta1, "\u2260", theta1[0])))
axis(1, at=c(-c, 0, +c), label=expression(c[o], theta1[0], c[u]), cex.axis=1.2)
lines(c(-c, -c), c(0, dnorm(-c)))
lines(c(+c, +c), c(0, dnorm(+c)))
lines(c(min(x),-c), c(-0.01,-0.01), col="red", lwd=5, lend=1)
lines(c(+c,max(x)), c(-0.01,-0.01), col="red", lwd=5, lend=1)
lines(c(-c,+c), c(-0.01,-0.01), col="blue", lwd=5, lend=1)
legend("topleft", expression(paste("Nicht-Ablehnungsbereich ", H[0]), 
                                   paste("Ablehnungsbereich ", H[0])),
       col=c("blue", "red"), lwd=5, cex=1.2)
text(0, dnorm(0), "Teststatistik V", pos=3, cex=1.5)
box()

plot(x,y, type="l", axes=F, xlab="v", ylab="f(v)", ylim=c(0,0.5), cex.main=2, cex.lab=1.2,
     main=expression(paste("Rechtsseitiger Test:  ", H[0], ": ", theta1, "\u2264", theta1[0], "  vs.  ", H[1], ": ", theta1>theta1[0])))
axis(1, at=c(0, +c), label=expression(theta1[0], c), cex.axis=1.2)
lines(c(+c, +c), c(0, dnorm(+c)))
lines(c(+c,max(x)), c(-0.01,-0.01), col="red", lwd=5, lend=1)
lines(c(min(x),+c), c(-0.01,-0.01), col="blue", lwd=5, lend=1)
legend("topright", expression(paste("Nicht-Ablehnungsbereich ", H[0]), 
                                   paste("Ablehnungsbereich ", H[0])),
       col=c("blue", "red"), lwd=5, cex=1.2)
text(0, dnorm(0), "Teststatistik V", pos=3, cex=1.5)
box()

plot(x,y, type="l", axes=F, xlab="v", ylab="f(v)", ylim=c(0,0.5), cex.main=2, cex.lab=1.2,
     main=expression(paste("Linksseitiger Test:  ", H[0], ": ", theta1>=theta1[0], "  vs.  ", H[1], ": ", theta1<theta1[0])))
axis(1, at=c(-c, 0), label=expression(c, theta1[0]), cex.axis=1.2)
lines(c(-c, -c), c(0, dnorm(-c)))
lines(c(min(x),-c), c(-0.01,-0.01), col="red", lwd=5, lend=1)
lines(c(-c,max(x)), c(-0.01,-0.01), col="blue", lwd=5, lend=1)
legend("topleft", expression(paste("Nicht-Ablehnungsbereich ", H[0]), 
                                   paste("Ablehnungsbereich ", H[0])),
       col=c("blue", "red"), lwd=5, cex=1.2)
text(0, dnorm(0), "Teststatistik V", pos=3, cex=1.5)
box()
dev.off()

##
c2 <- -qnorm(2*pnorm(-c))
cairo_pdf("test2.pdf")

par(mfrow=c(3,1))
plot(x,y, type="l", axes=F, xlab="v", ylab="f(v)", ylim=c(0,0.5), cex.main=2, cex.lab=1.2,
     main=expression(paste("Zweiseitiger Test:  ", H[0], ": ", theta1==theta1[0], "  vs.  ", H[1], ": ", theta1, "\u2260", theta1[0])))
axis(1, at=c(-c, 0, +c), label=expression(c[u], theta1[0], c[o]), cex.axis=1.2)
draw.polygon(x, min(x), -c, col="red")
draw.polygon(x, -c, +c,     col="blue")
draw.polygon(x, +c, max(x), col="red")
text(0, dnorm(0), "Teststatistik V", pos=3, cex=1.5)
text.black(-2.5, expression(alpha/2))
text.black(+2.5, expression(alpha/2))
text.blue(0, expression(1-alpha))
box()

plot(x,y, type="l", axes=F, xlab="v", ylab="f(v)", ylim=c(0,0.5), cex.main=2, cex.lab=1.2,
     main=expression(paste("Rechtsseitiger Test:  ", H[0], ": ", theta1, "\u2264", theta1[0], "  vs.  ", H[1], ": ", theta1>theta1[0])))
axis(1, at=c(0, +c2), label=expression(theta1[0], c), cex.axis=1.2)
draw.polygon(x, min(x), +c2, col="blue")
draw.polygon(x, +c2, max(x), col="red")
polygon(c(-0.4,-0.4,0.4,0.4), c(0.05,0.15,0.15,0.05), col="white")
text(0, dnorm(0), "Teststatistik V", pos=3, cex=1.5)
text.black(+2.5, expression(alpha))
text.blue(0, expression(1-alpha))
box()

plot(x,y, type="l", axes=F, xlab="v", ylab="f(v)", ylim=c(0,0.5), cex.main=2, cex.lab=1.2,
     main=expression(paste("Linksseitiger Test:  ", H[0], ": ", theta1>=theta1[0], "  vs.  ", H[1], ": ", theta1<theta1[0])))
axis(1, at=c(-c2, 0), label=expression(c, theta1[0]), cex.axis=1.2)
draw.polygon(x, min(x), -c2, col="red")
draw.polygon(x, -c2, max(x), col="blue")
text(0, dnorm(0), "Teststatistik V", pos=3, cex=1.5)
text.black(-2.5, expression(alpha))
text.blue (0, expression(1-alpha))
box()
dev.off()

###
cairo_pdf("test3.pdf")
par(mfrow=c(3,1))

d <- 0.25
x <- (-400:400)/100
y1 <- dnorm(x, d)
y2 <- dnorm(x, -d)

plot(x,y2, type="l", axes=F, xlab="v", ylab="f(v)", ylim=c(0,0.5), cex.main=2, cex.lab=1.2,
     main=expression(paste("Zweiseitiger Test:  ", H[0], ": ", theta1==theta1[0], "  vs.  ", H[1], ": ", theta1, "\u2260", theta1[0])))
axis(1, at=c(-d, d-c, d, d+c), label=expression(theta1[1], c[o], theta1[0], c[u]), cex.axis=1.2)
draw.polygon(x, d-c, d+c, "green", -d) 
draw.polygon(x, min(x), d-c, "red", +d) 
draw.polygon(x, d+c, max(x), "red", +d) 
lines(x, y1)
lines(c(d,d), c(0,dnorm(0)))
lines(c(-d,-d), c(0,dnorm(0)))
text(d, dnorm(0),  expression(paste("Teststatistik V|", theta1[0])), pos=4, cex=1.5)
text(-d, dnorm(0),  expression(paste("V|", theta1[1])), pos=2, cex=1.5)
legend("topleft", c(expression(paste("Fehler 1. Art: ", alpha, "=P(\"", H[1], "\"|", H[0], ")")),
                    expression(paste("Fehler 2. Art: ", beta,  "=P(\"", H[0], "\"|", H[1], ")"))),
       fill=c("red", "green"))
box()

plot(x,y2, type="l", axes=F, xlab="v", ylab="f(v)", ylim=c(0,0.5), cex.main=2, cex.lab=1.2,
     main=expression(paste("Rechtsseitiger Test:  ", H[0], ": ", theta1,"\u2264",theta1[0], "  vs.  ", H[1], ": ", theta1>theta1[0])))
axis(1, at=c(-d, c2+d, d), label=expression(theta1[1], c, theta1[0]), cex.axis=1.2)
draw.polygon(x, d+c2, max(x), "red", +d) 
draw.polygon(x, min(x), d+c2, "green", -d) 
lines(x, y1)
lines(c(d,d), c(0,dnorm(0)))
lines(c(-d,-d), c(0,dnorm(0)))
text(d, dnorm(0),  expression(paste("Teststatistik V|", theta1[0])), pos=4, cex=1.5)
text(-d, dnorm(0),  expression(paste("V|", theta1[1])), pos=2, cex=1.5)
legend("topleft", c(expression(paste("Fehler 1. Art: ", alpha, "=P(\"", H[1], "\"|", H[0], ")")),
                    expression(paste("Fehler 2. Art: ", beta,  "=P(\"", H[0], "\"|", H[1], ")"))),
       fill=c("red", "green"))
box()

plot(x,y2, type="l", axes=F, xlab="v", ylab="f(v)", ylim=c(0,0.5), cex.main=2, cex.lab=1.2,
     main=expression(paste("Linksseitiger Test:  ", H[0], ": ", theta1>=theta1[0], "  vs.  ", H[1], ": ", theta1<theta1[0])))
axis(1, at=c(-d, -c2-d, d), label=expression(theta1[1], c, theta1[0]), cex.axis=1.2)
draw.polygon(x, -d-c2, max(x), "green", -d) 
draw.polygon(x, min(x), -d-c2, "red",   +d) 
lines(x, y1)
lines(c(d,d), c(0,dnorm(0)))
lines(c(-d,-d), c(0,dnorm(0)))
text(d, dnorm(0),  expression(paste("Teststatistik V|", theta1[0])), pos=4, cex=1.5)
text(-d, dnorm(0),  expression(paste("V|", theta1[1])), pos=2, cex=1.5)
legend("topleft", c(expression(paste("Fehler 1. Art: ", alpha, "=P(\"", H[1], "\"|", H[0], ")")),
                    expression(paste("Fehler 2. Art: ", beta,  "=P(\"", H[0], "\"|", H[1], ")"))),
       fill=c("red", "green"))
box()

dev.off()

#####
cz <- 1.96
ce <- 1.65
x <- (-350:350)/100
y <- dnorm(x)
#
pdf("zweiseitig.pdf", height=1, width=4)
par(mar=c(0,0,0,0))
plot(x,y, type="l", axes=F, xlab="", ylab="")
lines(c(min(x), max(x)), c(0,0))
draw.polygon(x, min(x), -cz, "red") 
draw.polygon(x, cz, max(x), "red") 
dev.off()
pdf("linksseitig.pdf", height=1, width=4)
par(mar=c(0,0,0,0))
plot(x,y, type="l", axes=F, xlab="", ylab="")
lines(c(min(x), max(x)), c(0,0))
draw.polygon(x, min(x), -ce, "red") 
dev.off()
pdf("rechtsseitig.pdf", height=1, width=4)
par(mar=c(0,0,0,0))
plot(x,y, type="l", axes=F, xlab="", ylab="")
lines(c(min(x), max(x)), c(0,0))
draw.polygon(x, ce, max(x), "red") 
dev.off()
