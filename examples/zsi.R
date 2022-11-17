plotdensity <- function(x, y, from, to, col) {
  sel <- seq(length(x))[(x>=from) & (x<=to)]
             print(sel)
  xpol <- c(x[sel],x[rev(sel)])
  ypol <- c(y[sel],rep(0, length(sel)))
  polygon(xpol, ypol, col=col)
}

online <- F

# Simuliere Grundgesamtheit
set.seed(0)
n <- 2000
nsam <-100 

income <- rchisq(n, 4)
breaks <- 0:ceiling(max(income))

hhei <- 0.25

if (!online) pdf("zsi_gg.pdf", width=10, height=6)
h<-hist(income, breaks, freq=F, xlab="Einkommen", main="Einkommensverteilung in der GG", ylim=c(0, hhei), col="gray")
abline(v=mean(income), col="red", lwd=2)
 # expr(c(mu,sigma), 
minc <- round(mean(income), 3)
sinc <- round(sd(income), 3)
xpos <- minc
ypos <- floor(100*hhei)/100
text(xpos, ypos, bquote(mu == .(minc)), pos=4, col="red")
text(xpos, ypos-0.01, bquote(sigma == .(sinc)), pos=4, col="blue")
if (!online) dev.off()

# Stichprobe
if (!online) pdf("zsi_sp.pdf", width=10, height=6)
par(mfrow=c(1,2))
sincome <- sample(income, nsam, replace=T)
h<-hist(sincome, breaks, freq=F, xlab="Einkommen", main="Einkommensverteilung in einer Stichprobe", sub=sprintf("n=%i", nsam), ylim=c(0, hhei))
abline(v=mean(income), col="red", lwd=2)
text(xpos, ypos, bquote(mu == .(minc)), pos=4, col="red")
msam <- round(mean(sincome), 3)
abline(v=msam, col="green", lwd=2)
text(xpos, ypos-0.01, bquote(bar(x) == .(msam)), pos=4, col="green")
sincome <- sample(income, nsam, replace=T)
h<-hist(sincome, breaks, freq=F, xlab="Einkommen", main="Einkommensverteilung in einer Stichprobe", sub=sprintf("n=%i", nsam), ylim=c(0, hhei))
abline(v=mean(income), col="red", lwd=2)
text(xpos, ypos, bquote(mu == .(minc)), pos=4, col="red")
msam <- round(mean(sincome), 3)
abline(v=msam, col="green", lwd=2)
text(xpos, ypos-0.01, bquote(bar(x) == .(msam)), pos=4, col="green")
if (!online) dev.off()


# Stichprobenvariable j
j <-7
B <- 10000
xj <- rep(NA, B)
for (i in 1:B) {
  sincome <- sample(income, nsam, replace=T)
  xj[i] <- sincome[j]
}
par(mfrow=c(2,1))
hist(income, breaks, freq=F, xlab="Einkommen", main="Einkommensverteilung in der GG", ylim=c(0, hhei), col="gray")
hist(xj, breaks, freq=F, xlab="Einkommen", main=sprintf("Verteilung der Stichprobenvariable X%i", j), ylim=c(0, hhei), lwd=2)


# Stichprobenmittelwerte
if (!online) pdf("zsi_mean.pdf", width=10, height=6)
par(mfrow=c(1,1))
B <- 10000
mB <- rep(NA, B)
for (i in 1:B) {
  sincome <- sample(income, nsam, replace=T)
  mB[i] <- mean(sincome)
}

sincome <- sample(income, nsam, replace=T)
h<-hist(mB, breaks=20, freq=F, xlab=expression(bar(x)), main="Verteilung der Stichprobenmittelwerte", col="green", sub=sprintf("B=%i", B), xlim=c(2.75,5.25))
abline(v=mean(income), col="red", lwd=2)
text(xpos, 0.1, bquote(mu == .(minc)), pos=4, col="red")

xp <- min(mB)+(0:100)/100*diff(range(mB))
yp <- dnorm(xp, mean=mean(income), sd=sd(income)/sqrt(nsam))
sB <- round(sinc/sqrt(nsam), 3)
lines(xp, yp, lwd=2)
text(xpos, max(h$density), bquote("N(" * .(minc) *  "; "  * .(sB) * " = " * .(sinc) * "/" * sqrt(.(nsam)) * ")" ),pos=4)
if (!online) dev.off()

# Zentrales Schwankungsintervall
if (!online) pdf("zsi_theo.pdf", width=10, height=6)
q <- c(0.999, 0.99, 0.95, 0.9, 0.8)
nq <- length(q)
qn <- qnorm(1-(1-q)/2)
col <- gray((nq:0)/nq)
plot(xp, yp, type="l", main="Zentrales Schwankungsintervall")
for (i in seq(q)) {
  xu <- minc - qn[i]*sinc/sqrt(nsam)
  xo <- minc + qn[i]*sinc/sqrt(nsam)
  plotdensity(xp,yp, xu, xo, col=col[i])
}
abline(v=mean(income), col="red", lwd=2)
text(xpos, 0.1, bquote(mu == .(minc)), pos=4, col="red")
text(xpos, max(yp), bquote("N(" * .(minc) *  "; "  * .(sB) * ")" ),pos=4)
legend("topright", legend=sprintf("%.1f%% = [%.3f; %.3f]", 100*q, minc-qn*sinc/sqrt(nsam),  minc+qn*sinc/sqrt(nsam)), fill=col)
if (!online) dev.off()

# Zentrales Schwankungsintervall mit Stichprobe
if (!online) pdf("zsi_prak.pdf", width=10, height=6)
par(mfrow=c(1,2))
xu <- minc - 1.96*sinc/sqrt(nsam)
xo <- minc + 1.96*sinc/sqrt(nsam)
sincome <- sample(income, nsam, replace=T)
h<-hist(sincome, breaks, freq=F, xlab="Einkommen", main="95% Schwankungsintervall", sub=sprintf("n=%i", nsam), xlim=c(0,10), ylim=c(0, hhei))
abline(v=mean(income), col="red", lwd=2)
text(xo, ypos, bquote(mu == .(minc)), pos=4, col="red")
msam <- round(mean(sincome), 3)
abline(v=msam, col="green", lwd=2)
text(xo, ypos-0.01, bquote(bar(x) == .(msam)), pos=4, col="green")
sincome <- sample(income, nsam, replace=T)
abline(v=xu, col="brown")
abline(v=xo, col="brown")
text(xu, ypos-0.02, round(xu, 3), pos=2, col="brown")
text(xo, ypos-0.02, round(xo, 3), pos=4, col="brown")

h<-hist(sincome, breaks, freq=F, xlab="Einkommen", main="95% Schwankungsintervall", sub=sprintf("n=%i", nsam), xlim=c(0,10), ylim=c(0, hhei))
abline(v=mean(income), col="red", lwd=2)
text(xo, ypos, bquote(mu == .(minc)), pos=4, col="red")
msam <- round(mean(sincome), 3)
abline(v=msam, col="green", lwd=2)
text(xo, ypos-0.01, bquote(bar(x) == .(msam)), pos=4, col="green")
xu <- minc - 1.96*sinc/sqrt(nsam)
xo <- minc + 1.96*sinc/sqrt(nsam)
abline(v=xu, col="brown")
abline(v=xo, col="brown")
text(xu, ypos-0.02, round(xu, 3), pos=2, col="brown")
text(xo, ypos-0.02, round(xo, 3), pos=4, col="brown")
if (!online) dev.off()



# Konfidenzintervall
if (!online) pdf("zsi_konf.pdf", width=10, height=6)
par(mfrow=c(1,2))
sincome <- sample(income, nsam, replace=T)
msam <- round(mean(sincome), 3)
xu <- msam - 1.96*sinc/sqrt(nsam)
xo <- msam + 1.96*sinc/sqrt(nsam)

h<-hist(sincome, breaks, freq=F, xlab="Einkommen", main="95% Konfidenzintervall", sub=sprintf("n=%i", nsam), xlim=c(0,10), ylim=c(0, hhei))
abline(v=mean(income), col="red", lwd=2)
text(xo, ypos, bquote(mu == .(minc)), pos=4, col="red")
abline(v=msam, col="green", lwd=2)
text(xo, ypos-0.01, bquote(bar(x) == .(msam)), pos=4, col="green")
abline(v=xu, col="green")
abline(v=xo, col="green")
text(xu, ypos-0.02, round(xu, 3), pos=2, col="green")
text(xo, ypos-0.02, round(xo, 3), pos=4, col="green")

sincome <- sample(income, nsam, replace=T)
msam <- round(mean(sincome), 3)
xu <- msam - 1.96*sinc/sqrt(nsam)
xo <- msam + 1.96*sinc/sqrt(nsam)

h<-hist(sincome, breaks, freq=F, xlab="Einkommen", main="95% Konfidenzintervall", sub=sprintf("n=%i", nsam), xlim=c(0,10), ylim=c(0, hhei))
abline(v=mean(income), col="red", lwd=2)
text(xo, ypos, bquote(mu == .(minc)), pos=4, col="red")
abline(v=msam, col="green", lwd=2)
text(xo, ypos-0.01, bquote(bar(x) == .(msam)), pos=4, col="green")
abline(v=xu, col="green")
abline(v=xo, col="green")
text(xu, ypos-0.02, round(xu, 3), pos=2, col="green")
text(xo, ypos-0.02, round(xo, 3), pos=4, col="green")
if (!online) dev.off()
