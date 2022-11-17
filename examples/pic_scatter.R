library("foreign")
x <- read.spss("Europa.sav", to.data.frame = T)
pdf("pic_scatter.pdf")
plot(lem~sb, data=x, pch=19,
     xlab="Proz. Anteil der Stadtbevölkerung",
     ylab="Mittlere Lebenserwartung der Männer",
     cex.lab=2, cex.axis=2)
xs <- subset(x, (x$land=="ALBA") | (x$land=="NORW") | (x$land=="JUGO"))
text(xs$sb, xs$lem, labels=xs$land, pos=4, cex=1.5)
dev.off()