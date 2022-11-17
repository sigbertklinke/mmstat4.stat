size <- 10
p    <- 0.4
x <- rbinom(50, size, 0.4)
# 5.1 4.1 4.1 2.1
pdf('ggx.pdf', height=3.5)
op<-par(mar=0)
plot(table(x), type='h', main="", ylab="", xlab="", axes=FALSE)
box()
dev.off()
pdf('mlx.pdf', height=3.5)
x <- dpois(0:max(x), lambda = size*p)
op<-par(mar=0)
plot(x, type='h', main="", ylab="", xlab="", axes=FALSE)
box()
dev.off()