x <- read.csv("hhberlin_2017.csv", header=T)

pdf("hhberlin.pdf", width=10, height=7)
layout(matrix(c(1,2,3,4,5,5,5,5), 2, 4, byrow=T))
ind <- trunc(seq(1, nrow(x), length.out=4))
vn <- c("1", "2", "3", "4", "4<")
col <- c("gray20", "gray30", "gray40", "gray50", "gray60")
for (i in ind) {
  v <- as.numeric(x[i, 2:6])
  v <- 100*v/sum(v)
  names(v)<-vn
  barplot(v, ylim=c(0,60), main=x[i,1], col=col)  
}
plot(x[,1], x[,2], type="l", ylim=range(rowSums(x[,2:6])), xlab="Jahr", ylab="Haushalte (in Tsd.)", main="Privathaushalte in Berlin (oben: Anteile, unten: Gesamtsumme)", col=col[1], lwd=3)
for (i in 3:6) {
  lines(x[,1], rowSums(x[,2:6]), lwd=3)
}
dev.off()