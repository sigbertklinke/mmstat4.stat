# https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Haushalte-Familien/Tabellen/lrbev05.html
hh <- cbind(
  c(27.6, 28.4, 18.3, 15.0, 10.6),
  c(36.1, 33.4, 14.7, 11.5, 4.4),
  c(42.3, 33.2, 11.9, 9.1, 3.5),
  c(7.1, 14.7, 17, 16.8, 44.4),
  c(6.7, 17.7, 22.5, 19.7, 33.3),
  c(19.4, 25.3, 23, 16.2, 16.1)
)
colnames(hh) <- c( "1975", "2000", "2019", "1900", "1925", "1950")
row.names(hh) <- c("1", "2", "3", "4", "5+")
hh <- as.table(hh)
library("lattice")
pdf("haushalt.pdf", width=10, height=7)
barchart(hh, horizontal=FALSE, groups=FALSE, ylab="Prozent", main="Anteil Haushaltsgrößen Deutschland", labels=TRUE,
         ylim = c(0,50),
         panel=function(...) {
           args <- list(...)
           panel.text(args$x, args$y, args$y, pos=3, cex=0.75)
           panel.barchart(...)
         }
         )
dev.off()
