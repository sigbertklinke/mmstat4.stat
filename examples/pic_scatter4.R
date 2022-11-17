library("rio")
x <- import("https://shinyapps.wiwi.hu-berlin.de/d/Europa.sav")
pdf("pic_scatter4.pdf")
pairs(~sb+lem+lew+ks, data=x, pch=19, main="Streudiagramm-Matrix",
      cex.lab=2, cex.axis=2, cex.main=2)
dev.off()