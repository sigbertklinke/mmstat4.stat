noten <- c(rep(10, 57), rep(25, 93), rep(35, 92), rep(40, 29), rep(50, 3))
pdf("pic_hist.pdf")
hist(noten, breaks=c(0,20,30,37,46,51), main="Histogramm nach Notenklassen",
    xlab="Punkte", ylab="Häufigkeitsdichte",
    cex.main=2, cex.axis=2, cex.lab=2)
dev.off()