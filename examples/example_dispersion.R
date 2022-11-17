library("MASS")
x <- Boston$crim
IQR(x)           # Interquartilsabstand
mad(x)           # mittlere absolute Abweichung
IQR(x)/median(x) # Quartilsdispersionskoeffizient

# Reskalierung, weil R die Stichprobenvarianz berechnet.
n      <- length(x)
sigma2 <- var(x)*((n-1)/n) 
sigma2                 # Varianz 
sqrt(sigma2)           # Standardabweichung
sd(Boston$crim)        #
sqrt(sigma2)/mean(x)   # Variationskoeffizient
diff(range(x))         # Spannweite