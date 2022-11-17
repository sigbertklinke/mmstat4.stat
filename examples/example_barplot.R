x <- read.csv2("haushalte_berlin.csv")
jahr <- c(1991, 2014)
#
row  <- x[,1]==jahr
hh   <- as.matrix(100*x[row, 3:7]/x[row,2], nrow=5)
colnames(hh) <- c(1:4, "5+")
#
barplot(hh, main="Anteil x Personenhaushalte", 
        xlab="Haushaltsgroesse", ylab="Anteil (in %)",
        beside=T, legend.text=jahr)