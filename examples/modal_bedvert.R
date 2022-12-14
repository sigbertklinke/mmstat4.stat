x <- c(0.79, 0.11, 0.02, 0.08, 0.27, 0.23, 0.18, 0.33, 0.05, 0.18, 0.35, 0.42, 0.01, 0.12, 0.44, 0.43, 0.00, 0.03, 0.48, 0.49)
x <- matrix(x, ncol=5)
x <- sweep(x, 2, colSums(x), '/')
w <- c(0.31, 0.22, 0.12, 0.17, 0.17)
w <- w/sum(w)
xw <- rbind(x[,1]*w[1], x[,2]*w[2],  x[,3]*w[3],  x[,4]*w[4],  x[,5]*w[5] )
r  <- c(0.5, 2.0, 4.0, 7.5, (6-w[1]*0.5-w[2]*2-w[3]*4-w[4]*7.5)/w[5])
colSums(xw)
rowSums(xw)
wx<-sweep(xw, 2, colSums(xw), '/')
colSums(wx*r)
xw
colSums(xw)
rowSums(xw)
e<-outer(rowSums(xw),colSums(xw))
round(e, 2)
tab <- as.table(50000*xw)
colnames(tab) <- c('Fuss', 'Rad', 'ÖPNV', 'MIV')
rownames(tab) <- c('0-1 km', '1-3 km', '3-5 km', '5-10 km', '10+ km')
pdf("modal_bedvert.pdf", width=8, height=5)
plot(tab, las=1, dir=c("h", "v"), col=c("green", "orange", "blue", "red"), main="Mosaikplot von Weglängen und Verkehrsmittel")
dev.off()

cs<-chisq.test(tab)
cs$statistic
cu<-sqrt(cs$statistic/(50000+cs$statistic))
cu
cu*sqrt(4/3)