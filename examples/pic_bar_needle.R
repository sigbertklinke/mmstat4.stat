note <- c(3, 29, 92, 93, 57)
names(note) <- 1:5
notetab <- as.table(note)
pdf("pic_bar_needle.pdf", width=10, height=6)
par(mfrow=c(1,2))
barplot(prop.table(notetab), ylab="f(x)", main="Säulendiagramm", cex.main=2)
plot(prop.table(notetab), ylab="f(x)", main="Stabdiagramm", cex.main=2)
dev.off()