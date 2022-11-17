library("mmstat4")
data(hhB)
#
jahr <- c(1991, 2014)
row  <- hhB[,1]==jahr
hh   <- as.matrix(100*hhB[row, 3:7]/hhB[row,2], nrow=5)
colnames(hh) <- c(1:4, "5+")
#
barplot(hh, main="Anteil x Personenhaushalte (Berlin)",
        xlab="Haushaltsgroesse", ylab="Anteil (in %)",
        beside=T, legend.text=jahr)
#
plot(hhB$Jahr, hhB$Privathaushalte, type="b",
     main="Anzahl Privathaushalte (Berlin)",
     xlab="", ylab="Privathaushalte (in Tsd.)")
