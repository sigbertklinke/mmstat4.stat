wurf <- function (feld, pos=0, tiefe=1) {
  p <- 1/36
  if (tiefe>3) {
    feld[36] <- feld[36] + p^3
    return (feld)
  }
  for (i in 1:6) {
    for (j in 1:6) {
      if (i==j) {
        feld <- wurf(feld, pos+i+j, tiefe+1)
      } else {
        feld[pos+i+j] <- feld[pos+i+j]  + p^tiefe
      }
    }
  }
  return (feld)
}

feld <- rep(0, 36)
feld <- wurf(feld)
sum(feld)
plot(feld, type="h")