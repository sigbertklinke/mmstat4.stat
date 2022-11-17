library("foreign")
allbus <- read.spss("ALLBUScompact2012.sav", to.data.frame = T)
levels(allbus$V2) <- substring(levels(allbus$V2), 8)

pdf("pic_boxplot1.pdf", width=10, height=6)
west   <- subset(allbus, allbus$V5=="ALTE BUNDESLAENDER")
boxplot(V815~V2, data=west, ylim=c(0, 10000), 
        xlab="Jahr", ylab="Nettoeinkommen (EUR)",
        main="Alte Bundesländer", col="red")
dev.off()

pdf("pic_boxplot2.pdf", width=10, height=6)
reunion <- subset(allbus, as.integer(allbus$V2)>6)
reunion$V2 <- factor(reunion$V2)
levels(reunion$V5) <- c("W", "O") 
boxplot(V815~V5*V2, data=reunion, ylim=c(0, 10000), 
        xlab="Jahr", ylab="Nettoeinkommen (EUR)",
        main="Vergleich alte/neue Bundesländer", col=c("red", "green"))
dev.off()
