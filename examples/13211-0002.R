library("gdata")
x   <- read.xls("13211-0002_2017.xls", na.strings="...")
lvl <- levels(x[,1])
col <- rainbow(2*length(lvl))
pdf("13211-0002.pdf", width=12, height=8)
plot(0,0, xlab="Jahr", ylab="log10(Anzahl)", ylim=c(3.75,6.75), xlim=2005+c(0, years)) 
i   <- 1
legend <- c()
for (gebiet in lvl) {
  xg <- subset(x, x[,1]==gebiet)
  t <- 2005+(0:(length(xg[,4])-1))/12
  lines(t, log10(xg[,4]), col=col[i], lwd=2)
  lines(t, log10(xg[,7]), col=col[i+1], lwd=2)
  legend <- c(legend, paste("Arbeitslose", gebiet), paste("Stellen", gebiet))
  i <- i+2
}
legend("bottomright", legend=legend, col=col, lwd=2)
dev.off()
