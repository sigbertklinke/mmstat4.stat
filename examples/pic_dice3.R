rpdf <- 'pic_dice3.pdf'
# 
pdf(rpdf, width=10, height=7)
par(mfrow=c(1,2))
tabe <- rep(1/8, 8)
names(tabe) <- c("KKK (3)","KKZ (2)","KZK (2)","KZZ (1)","ZKK (2)","ZKZ (1)","ZZK (1)","ZZZ (0)")
plot(as.table(tabe), main="Elementarereignisse", ylab="Wahrscheinlichkeit", axes=F, ylim=c(0,0.4))
points(1:8, tabe, pch=19)
axis(1, at=1:8, labels=names(tabe), las=2)
axis(2)
box()
tabk <- c(1,3,3,1)/8
names(tabk) <- 0:3
plot(as.table(tabk), main="Anzahl Koepfe", ylab="Wahrscheinlichkeit", ylim=c(0,0.4))
points(0:3, tabk, pch=19)
dev.off()