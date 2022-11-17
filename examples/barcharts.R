tab <- matrix(c(240,160,30,37,40,120,90,30,7,32,70,90,30,6,18), ncol=3)
rownames(tab) <- c("Arbeiter", "Angestellter", "Beamter", "Landwirt", "sonst. freier Beruf")
colnames(tab) <- c("kaum", "gelegentlich", "regelmäßig")
dfreq <- as.data.frame(as.table(tab))

library(latticeExtra)

pdf("barcharts1.pdf")
cloud(Freq~Var1+Var2, dfreq, panel.3d.cloud=panel.3dbars, col.facet='grey', 
			xbase=0.4, ybase=0.4, scales=list(arrows=FALSE, col=1), 
			par.settings = list(axis.line = list(col = "transparent")),
			xlab = "Berufsgruppe", ylab = "Sportliche Betätigung", zlab="Abs. Häufigkeit")
dev.off()

pdf("barcharts2.pdf")
col <- gray((0:4)/5)
bp <- barplot(tab,  beside=TRUE, xlab="Sportliche Betätigung", ylab="Absolute Häufigkeit")
legend("topright", legend=rownames(tab), fill=col, title="Berufsgruppe")
dev.off()

pdf("barcharts3.pdf")
plot(as.table(tab), main="Mosaikplot", xlab="Berufsgruppe",  ylab = "Sportliche Betätigung")
dev.off()

pdf("barcharts4.pdf")
col <- gray((0:3)/4)
bp <- barplot(t(tab),  beside=TRUE, xlab="Berufsgruppe", ylab="Absolute Häufigkeit")
legend("topright", legend=colnames(tab), fill=col, title="Sportliche Betätigung")
dev.off()

pdf("barcharts5.pdf")
plot(as.table(t(tab)), main="Mosaikplot", ylab="Berufsgruppe",  xlab = "Sportliche Betätigung")
dev.off()
