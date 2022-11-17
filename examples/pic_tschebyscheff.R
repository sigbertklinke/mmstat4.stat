x   <- seq(-3, 3, length.out=301)
lim <- 1.1111
pdf("pic_tschebyscheff.pdf", width=10, height=5)
plot(x, dnorm(x), xlab="", ylab="", type="l")
px <- c(min(x), x[x< -lim], -lim, -lim)
py <- c(0, dnorm(x[x< -lim]), dnorm(-lim), 0)
polygon(px, py, col="green")
#px <- c(-lim, -lim, x[(x>-lim) & (x<lim)], lim, lim)
#py <- c(0, dnorm(-lim), dnorm(x[(x>-lim) & (x<lim)]), dnorm(lim), 0)
#polygon(px, py, col="lightblue")
px <- c(lim, lim, x[x>lim], max(x))
py <- c(0, dnorm(lim), dnorm(x[x>lim]), 0)
polygon(px, py, col="green")#
lexpr <- vector("expression", 5) 
for (k in 1:5) 
  lexpr[k] = substitute(expression(paste("k=", i %=>% 1/k^2, "=", j)), 
                        list(i=k, j=round(1/k^2,2)))[2]
legend("topright", legend=lexpr)
dev.off()