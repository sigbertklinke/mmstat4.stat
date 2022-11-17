levels <- c(1, 1.3, 1.7, 2, 2.3, 2.7, 3, 3.3, 3.7, 4, 5)
noten  <- ordered(c(1.3, 3, 2.7, 2, 5), levels=levels)
noten
# Metrisch?
is.numeric(noten)
# Ordinal?
is.ordered(noten)
# Nominal?
is.factor(noten)
# Falsches Skalenniveau
mean(noten) 
# Aendere Skalenniveau
noten_num <- as.numeric(levels(noten)[noten])
noten_num
# Kein Fehler, da das Skalenniveau nun Summation zulaesst.
mean(noten_num) 