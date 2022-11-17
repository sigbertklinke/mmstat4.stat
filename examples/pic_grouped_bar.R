mat <- matrix(c(199, 1, 499, 99301), ncol=2)
rownames(mat) <- c("HIV-Test positiv", "HIV-Test negativ")
colnames(mat) <- c("HIV-Infektion vorhanden", "HIV-Infektion nicht vorhanden")
tab <- as.table(mat)

pdf("pic_grouped_bar1.pdf", width = 10)
barplot(prop.table(tab, 2), beside=T, legend.text = T, 
        cex.names=2, cex.axis=2, cex.lab=2,
        args.legend = list(x="center", cex=2))
dev.off()

pdf("pic_grouped_bar2.pdf", width = 10)
barplot(t(prop.table(tab, 1)), beside=T, legend.text = T, 
        cex.axis=2, cex.lab=2, cex.names = 2, 
        args.legend = list(x="topleft", cex=2))
dev.off()
