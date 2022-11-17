#install.packages("scatterplot3d")
library("MASS")
library("scatterplot3d")
scatterplot3d(Boston$crim, Boston$black, Boston$indus)