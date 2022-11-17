data(Boston, package="MASS")
# show first and last six observations
head(Boston)
tail(Boston)
# order by medv
index <- order(Boston$medv)
head(Boston[index,], n=10)
tail(Boston[index,], n=10)
