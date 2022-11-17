mat91 <- matrix(c(0.145, 0.519, 0.307, 0.025, 0.004, 0.111, 0.404, 0.447, 0.025, 0.011), ncol=2)
rownames(mat91) <- c("sehr gut", "gut", "befriedigend", "schlecht", "sehr schlecht")
colnames(mat91) <- c("West", "Ost")
tab91 <- as.table(mat91)

mat96 <- matrix(c(0.010, 0.124, 0.475, 0.325, 0.066, 0.006, 0.107, 0.515, 0.305, 0.067), ncol=2)
rownames(mat96) <- c("sehr gut", "gut", "befriedigend", "schlecht", "sehr schlecht")
colnames(mat96) <- c("West", "Ost")
tab96 <- as.table(mat96)

pdf("pic_allbus_gbar1.pdf", width=10, height=10)
barplot(t(tab91), beside=T, legend.text = T, 
        cex.names=2, cex.axis=2, cex.lab=2, cex.main=3,
        args.legend = list(x="topright", cex=2), main="1991")
dev.off()
pdf("pic_allbus_gbar2.pdf", width=10, height=10)
barplot(t(tab96), beside=T, legend.text = T, 
        cex.names=2, cex.axis=2, cex.lab=2, cex.main=3,
        args.legend = list(x="topright", cex=2), main="1996")
dev.off()

