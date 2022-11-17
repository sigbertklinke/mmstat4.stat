library("xtable")
B <- 5000
x <- runif(B)>0.5
h <- cumsum(x)
f <- h/(1:B)
pdf("pic_coin.pdf")
plot(10:B, f[-(1:9)], type="l", log="x",
    xlab="Anzahl der Würfe", ylab="f(Kopf)")
abline(h=0.5, lty="dotted")
dev.off()
n <- c(10,20,40,80,100,200,1000,5000)
tab <- cbind(n, h[n], f[n])
colnames(tab) <- c("n", "h(Kopf)", "f(Kopf)")
sink(file = "pic_coin.tex")
print(xtable(tab, digits=c(0,0,0,3)), format.args=list(decimal.mark = ","), include.rownames=F)
sink()