mat <- matrix(c(199, 1, 499, 99301), ncol=2)
rownames(mat) <- c("positiv", "negativ")
colnames(mat) <- c("vorhanden", "nicht vorhanden")
tab <- as.table(mat)

pdf("pic_mosaic1.pdf")
plot(tab, 2, xlab="HIV-Test", ylab="HIV-Infektion", main="")
dev.off()

pdf("pic_mosaic2.pdf")
plot(t(tab), ylab="HIV-Test", xlab="HIV-Infektion", main="")
dev.off()
