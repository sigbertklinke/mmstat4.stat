tab <- matrix(c(240,160,30,37,40,120,90,30,7,32,70,90,30,6,18), ncol=3)
tab <- outer(rowSums(tab), colSums(tab))/sum(tab)

rownames(tab) <- c("Arbeiter", "Angestellter", "Beamter", "Landwirt", "sonst. freier Beruf")
colnames(tab) <- c("kaum", "gelegentlich", "regelmäßig")
dfreq <- as.data.frame(as.table(tab))

pdf("pic_independent1.pdf")
col <- gray((0:3)/4)
bp <- barplot(t(tab),  beside=TRUE, xlab="Berufsgruppe", ylab="Absolute Häufigkeit")
legend("topright", legend=colnames(tab), fill=col, title="Sportliche Betätigung")
dev.off()

pdf("pic_independent2.pdf")
plot(as.table(t(tab)), main="Mosaikplot", ylab="Berufsgruppe",  xlab = "Sportliche Betätigung")
dev.off()

