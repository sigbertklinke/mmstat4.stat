png("pvalue.png", width=840, height=420, bg="transparent")
c  <- 1.65
v  <- (-300:300)/100
dv <- dnorm(v)
plot(v, dv, type="l", ylab="Dichte")
text(-0.5, 0.95*max(dv), "V~N(0, 1)", pos=2)

vp  <- 0.5
abline(v=vp)
text(vp, 0.95*max(dv), sprintf('v=%.1f', vp) , pos=4)
x  <- ((100*vp):300)/100
dx <- dnorm(x)
dx <- c(0, dx, 0)
x  <- c(min(x), x, max(x))
polygon(x, dx, col="grey")
text(vp, max(dx)/2, sprintf('p=%.4f', 1-pnorm(vp)), col="darkgrey", pos=2)

abline(v=c, col="red")
text(c, 0.7*max(dv), expression(paste('c=', z[1-alpha], '=1.65')), pos=4, col="red")

x <- ((100*c):300)/100
dx <- dnorm(x)
dx <- c(0, dx, 0)
x  <- c(min(x), x, max(x))
polygon(x, dx, border="red", density=10, col="red")
text(2.5, 0.03, expression(paste(alpha, "=0.05")), col="red", pos=3)

dev.off()