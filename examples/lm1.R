library("MASS")
lm1 <- lm(medv~lstat, data=Boston)
summary(lm1)