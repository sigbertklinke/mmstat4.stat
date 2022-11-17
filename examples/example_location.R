library("MASS")
# Mittelwert
mean(Boston$tax)
# Median
median(Boston$tax)
# 75% Quantil
quantile(Boston$tax, 0.75)
# Kein Befehl fuer den Modus
tab <- table(Boston$rad)
tab
max(tab)
which.max(tab)