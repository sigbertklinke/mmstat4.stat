library("rio")
x <- read.csv("airbnb_listings.csv")
xy <- cbind(as.numeric(substring(x$price, 2)), x$accommodates)
xy <- na.omit(xy)
xy <- xy[(xy[,2]>0) & (xy[,2]<9), ]
m <- tapply(xy[,1], xy[,2], mean)
s <- tapply(xy[,1], xy[,2], sd)
mr <- tapply(xy[,1], xy[,2], median)
iqr <- tapply(xy[,1], xy[,2], IQR)
v <- s/m
vr <- iqr/mr

n <- table(xy[,2])
tab <- rbind(n, m, s, v, mr, iqr, vr)
rownames(tab) <- c("$n$", "$\\bar{x}$", "$s$", "$v$", "$x_{0.5}$", "QA", "$v_r$")

library("xtable")
digits <- matrix(c(0,2,2,2,2,2,2), ncol=9, nrow=7)
xtab <- xtable(tab, digits=digits)
print.xtable(xtab, type="latex", file="variationskoeffizient.tex", sanitize.text.function=function(x){x})

pdf("variationskoeffizient.pdf", width=12.5, height=4)
par(mar=c(1.1, 4.1, 2.1, 1.1))
boxplot(xy[,1]~xy[,2], varwidth=TRUE, pch=19, cex=0.5, xlab="", ylab="Preis in US$", axes=FALSE)
axis(2)
axis(3, 1:8)
box()
dev.off()