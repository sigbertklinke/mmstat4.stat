library("MASS")
# Mittelwert
mean(Boston$crim)
# Standardabweichung
sd(Boston$crim)
plot(ecdf(Boston$crim))
# Standardisierung (Lineare Transformation)
z <- scale(Boston$crim)
# Mittelwert standardisiert
mean(z)
# Standardabweichung standardisiert
sd(z)
plot(ecdf(z))