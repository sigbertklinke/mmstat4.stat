f <- function(x) { exp(-x) / ((1+exp(-x))^2) }
integrate(f, 0, 1)