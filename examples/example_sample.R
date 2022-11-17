library("MASS")     # Boston Housing Daten
# https://stat.ethz.ch/R-manual/R-devel/
#            library/MASS/html/Boston.html
N <- nrow(Boston)   # Umfang Grundgesamtheit
n <- 51             # Umfang Stichprobe
# Ziehen ohne Zuruecklegen
sample(N, size=n)
# Ziehen mit Zuruecklegen
sample(N, size=n, replace=TRUE)