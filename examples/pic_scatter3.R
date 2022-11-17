library("foreign")
library("scatterplot3d")

x <- read.spss("Europa.sav", to.data.frame = T)
pdf("pic_scatter3.pdf")
scatterplot3d(x$ks, x$sb, x$lew, xlab="sb", ylab="ks", zlab="lew", pch=19, 
              main="3D-Streudiagramm", cex.lab=2, cex.axis=2, cex.main=2)
dev.off()


