
dt = 1

cs = c()
for (lambda in seq(from=.001,to=.1,by=.001)) {
  print(lambda)
  
  xs = raw.data[3000:4000,]
  Theta = features(data.frame(xs),2)
  dx = c()
  for (i in 1:ncol(xs)) {
    dx = cbind(dx,finDiff(xs[,i],dt))
  }
  
  Xi = sindylicious(xs,dx,Theta,lambda=lambda)
  #norm(Theta %*% XiD - dx)/norm(dx)
  
  xs = raw.data[4000:5000,]
  Theta = features(data.frame(xs),2)
  dx = c()
  for (i in 1:ncol(xs)) {
    dx = cbind(dx,finDiff(xs[,i],dt))
  }
  
  fits = as.vector(Theta %*% Xi)
  observed = as.vector(dx)
  observed = observed[fits!=0]
  fits = fits[fits!=0]
  cs = rbind(cs,data.frame(
    r=cor(fits,observed),
    l=lambda,
    n=sum(Xi>0)))
}
plot(cs$l,cs$r,type='b')
plot(cs$n,cs$r,type='b')
#plot(Theta %*% Xi,dx)


