cop    <- c(Fuss=0.17, Rad=0.30, ÖPNV=0.20, MIV=0.33)
wien   <- c(Fuss=0.26, Rad=0.07, ÖPNV=0.38, MIV=0.29)
muen   <- c(Fuss=0.22, Rad=0.39, ÖPNV=0.10, MIV=0.29)
wies   <- c(Fuss=0.25, Rad=0.03, ÖPNV=0.15, MIV=0.56)
hh     <- c(Fuss=0.28, Rad=0.12, ÖPNV=0.18, MIV=0.42)
ber    <- c(Fuss=0.31, Rad=0.13, ÖPNV=0.27, MIV=0.30)
innen  <- c(Fuss=0.35, Rad=0.18, ÖPNV=0.29, MIV=0.17)
aussen <- c(Fuss=0.29, Rad=0.10, ÖPNV=0.26, MIV=0.35)
london2016 <- c(Fuss=0.24, Rad=0.03, ÖPNV=0.35, MIV=0.36)
london2001 <- c(Fuss=0.24, Rad=0.02, ÖPNV=0.33, MIV=0.41)
col <- c("green", "orange", "blue", "red")
pdf("modalsplit.pdf", width=10, height=6)
par(mfrow=c(2,5), oma = c(0, 0, 3, 0))
barplot(as.table(cop), main="Kopenhagen 2014", ylim=c(0, 0.6), col=col, sub="623 Tsd. Einwohner, 86 km^2", las=2)
barplot(as.table(wien), main="Wien 2018", ylim=c(0, 0.6), col=col, sub="1898 Tsd. Einwohner, 415 km^2", las=2)
barplot(as.table(hh), main="Hamburg 2014", ylim=c(0, 0.6), col=col, sub="1841 Tsd. Einwohner, 755 km^2", las=2)
barplot(as.table(wies), main="Wiesbaden 2014", ylim=c(0, 0.6), col=col, sub="278 Tsd. Einwohner, 203 km^2", las=2)
barplot(as.table(london2001), main="London 2001", ylim=c(0, 0.6), col=col, sub="2765 Tsd. Einwohner, 319 km^2", las=2)
barplot(as.table(muen), main="Münster 2013", ylim=c(0, 0.6), col=col, sub="314 Tsd. Einwohner, 303 km^2", las=2)
barplot(as.table(ber), main="Berlin 2013 (gesamt)", ylim=c(0, 0.6), col=col, sub="3748 Tsd. Einwohner, 892 km^2", las=2)
barplot(as.table(innen), main="Berlin 2013 (innere Stadt)", ylim=c(0, 0.6), col=col, sub="1380 Tsd. Einwohner, 110 km^2", las=2)
barplot(as.table(aussen), main="Berlin 2013 (äußere Stadt)", ylim=c(0, 0.6), col=col, sub="2368 Tsd. Einwohner, 782 km^2", las=2)
barplot(as.table(london2016), main="London 2016", ylim=c(0, 0.6), col=col, sub="3231 Tsd. Einwohner, 319 km^2", las=2)

mtext("Modal splits für ausgewählte Städte", outer = TRUE, cex = 1.5)
dev.off()