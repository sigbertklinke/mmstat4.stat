hh <- ordered(c(rep("1", 360), 
                rep("2", 334), 
                rep("3", 147), 
                rep("4", 115), 
                rep("5", 44)),
              levels=1:5)
pdf("pic_ecdf.pdf")
plot(ecdf(hh), xlab="Haushaltsgrösse", main="Emp. Verteilungsfunktion",
    cex.main=2, cex.axis=2, cex.lab=2)
dev.off()