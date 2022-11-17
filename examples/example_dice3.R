n        <- 96
index    <- sample(1:8, 100, replace=T)
par(mfrow=c(1,2))
wuerfel3 <- c("KKK","KKZ","KZK","KZZ","ZKK","ZKZ","ZZK","ZZZ")
wuerfel3[index]
plot(table(wuerfel3[index]), main="Elementarereignisse",
		 ylab="Absolute Häufigkeit")
koepfe <- c(3, 2, 2, 1, 2, 1, 1, 0)
koepfe[index]
plot(table(koepfe[index]), main="Anzahl Köpfe",
		 ylab="Absolute Häufigkeit")