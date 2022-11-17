set.seed(8)
N<- 20
n<-4
x<-rnorm(N)
y<-rnorm(N)
r<- 1.1*max(sqrt(x^2+y^2))
par(mar=c(0,0,0,0))
plot(x,y, pch=19, xlim=c(-r,3.5*r), ylim=c(-1.1*r,r), axes=FALSE)

t<- (0:360)/180*pi
lines(r*cos(t),r*sin(t))

ind<-sample(1:N,n)
points(2.5*r+x[ind],y[ind], pch=19, col="red")
lines(2.5*r+r*cos(t), r*sin(t), col="red")
for(i in seq(ind)){
  lines(c(x[ind[i]],x[ind[i]]+2.5*r), c(y[ind[i]],y[ind[i]]))
  text(x[ind[i]]+2.5*r, y[ind[i]], as.expression(bquote(x[.(i)])), pos=4)
  text(1.25*r, y[ind[i]], as.expression(bquote(X[.(i)])), pos=3)
}
text(0, -r, "Grundgesamtheit", pos=1)
text(2.5*r, -r, "Stichprobe", pos=1, col="red")
