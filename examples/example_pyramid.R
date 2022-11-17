# install.packages("plotrix")
library("plotrix")
#
x <- read.csv("12411-0006.csv")
pyramid.plot(x$M/1e5, x$W/1e5, 
             labels=c(1:85, ">85"), labelcex=0.65, 
             lxcol="blue", rxcol="red", unit="in 100000",
             top.labels=c("Maennlich", "Alter", "Weiblich")
             )