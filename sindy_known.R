#
# coded by Rick Dale and Harish Bhat
# Adapted from the MATLAB code of Brunton et al. (2016), PNAS
#

#
# Logistic map
#

as = seq(from=2.1,to=3.99,by=.05)
xs = c()
x = runif(1)
for (a in as) {
  print(a)
  xs_temp = c()
  x = runif(1)
  for (i in 1:100) {
    #print(xs)
    x = a*x*(1-x)
    xs_temp = rbind(xs_temp,data.frame(r=a,x=x))
  }
  xs = rbind(xs,xs_temp)
}  

Theta = features(xs,3)[1:(nrow(xs)-1),]
dx = as.matrix(xs[2:nrow(xs),1:2])
Xi = mldivide(Theta,dx)
XiB = Xi
# iterative least squares solution
for (k in 1:5) {
  posinds = which(abs(Xi)<.25);  
  Xi[posinds]=0;
  XiC = Xi
  for (ind in 1:2) {
    Xitemp = Xi[,ind]
    posinds = which(abs(Xitemp)>=.25)
    Xi[posinds,ind] = mldivide(Theta[,posinds],dx[,ind])
  }
  XiD = Xi
}
Xi
norm(Theta %*% XiD - dx)/norm(dx)


#
# Lorenz attractor
#

library(crqa)
dt = .001
numsteps = 100000; dt = dt; sigma = 10; r = 28; b = 8/3;
xs = data.frame(lorenzattractor(numsteps, dt, sigma, r, b, plots=F))
colnames(xs) = list('x','y','z')

xs = xs[2000:nrow(xs),]
points3D(xs$x,xs$y,xs$z,type='l',col='black')
lambda = .02

Theta = features(xs,3)
dx = cbind(finDiff(xs$x,dt),finDiff(xs$y,dt),finDiff(xs$z,dt))
Xi = mldivide(Theta,dx)
XiB = Xi
# iterative least squares solution
for (k in 1:20) {
  posinds = which(abs(Xi)<lambda);  
  Xi[posinds]=0;
  XiC = Xi
  for (ind in 1:3) {
    Xitemp = Xi[,ind]
    posinds = which(abs(Xitemp)>=lambda)
    Xi[posinds,ind] = mldivide(Theta[,posinds],dx[,ind])
  }
  XiD = Xi
}
rownames(Xi) = colnames(Theta)
Xi
norm(Theta %*% XiD - dx)/norm(dx)

#
# two-well attractor behavior
#

deets = c()
for (i in 1:100) {
  print(i)
  x_trial = c()
  x = 0
  k = runif(1)-.5
  x_sum = 0
  for (j in 1:1000) {
    lx = x
    x = x + (-k + x - x^3) + rnorm(1)*.05
    x_sum = x_sum + x
    x_trial = rbind(x_trial,data.frame(t=j,x=x,x2=x^2,x3=x^3,k=k))
    if (abs(x_sum)>10) { break; }
  }
  deets = rbind(deets,x_trial)
}

Theta = deets[1:(nrow(deets)-1),]
dx = cbind(deets$x[2:nrow(deets)],deets$k[2:nrow(deets)])
Xi = sindylicious(dx,dx,as.matrix(Theta),lambda=.01);Xi  









