library("mmstat4")
data(hhD)
#
ylim <- c(0, max(hhD$Einpersonen, na.rm=T))
plot(hhD$Jahr, hhD$Einpersonen, xlab="", ylab="Anteil (in %)",
     main="Anteil x Personen Haushalte (Deutschland)",
     sub="Bis 1990 Frueheres Bundesgebiet",
     pch=19, cex=0.75, ylim=ylim)
#
points(hhD$Jahr, hhD$Zweipersonen, pch=19, cex=0.75, col="red")
points(hhD$Jahr, hhD$Dreipersonen, pch=19, cex=0.75, col="blue")
points(hhD$Jahr, hhD$Vierpersonen, pch=19, cex=0.75, col="green")
points(hhD$Jahr, hhD$'Fuenf und mehr Personen',
       pch=19, cex=0.75, col="orange")
#
legend("topleft", title="Haushalte mit", pch=19, cex=0.75,
       col=c("black", "red", "blue", "green", "orange"),
       legend=c("1 Person", "2 Personen", "3 Personen",
                "4 Personen", "5+ Personen"))

