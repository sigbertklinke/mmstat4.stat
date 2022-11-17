HIV  <- as.table(cbind(vorhanden=c(995,5),
								 nicht_vorhanden=c(5,995)))
# Kontingenztabelle mit Randverteilungen
addmargins(HIV)
# chi-quadrat Test, speichern der Ergebnisse
kont <- chisq.test(HIV)
# Erwartete Hauefigkeiten
kont$expected
# Quadratische Kontingenz
qk <- kont$statistic
qk
# Kontingenzkoeffizient
kk <- sqrt(qk/(qk+sum(HIV)))
kk
# Korrigierter Kontingenzkoeffizient
cs <- min(nrow(HIV), ncol(HIV))
KK <- kk*sqrt(cs/(cs-1))
KK