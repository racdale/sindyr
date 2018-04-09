#########################################################################
#
# coded by Rick Dale and Harish Bhat
# Adapted from the MATLAB code of Brunton et al. (2016), PNAS
#
#########################################################################
setwd('~/Dropbox/new.projects/multiModal/Dale and Bhat/')
source('R_code/some_functions.R')
library(pracma)

########################################################################
#
# Logistic map
#
########################################################################

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
    xs_temp = rbind(xs_temp,data.frame(a=a,x=x))
  }
  xs = rbind(xs,xs_temp)
}  
xs[1:4,]
dx = as.matrix(xs[2:nrow(xs),])
xs = xs[1:(nrow(xs)-1),]

# 
# let's get some plots
#

pdf('figures/figure_2_a.pdf',width=5,height=5)
plot(xs[,1],type='p',cex=.5,pch=15,xlab='t',ylab='a') # a
dev.off()
pdf('figures/figure_2_dx.pdf',width=5,height=5)
plot(dx,type='p',cex=.5,pch=15,xlab='t',ylab='dx') # dx
dev.off() 
pdf('figures/figure_2_x.pdf',width=5,height=5)
plot(xs[,2],type='p',cex=.5,pch=15,xlab='t',ylab='x') # x
dev.off()
pdf('figures/figure_2_features.pdf',width=4,height=4)
Theta = features(xs,3) # grid of features
par(mfrow=c(3,3),oma = c(2,0,0,0) + 0.1,mar = c(1,1,1,1) + 0.1)
for (i in 2:ncol(Theta)) {
  plot(Theta[,i],xlab='t',main=gsub(':','',colnames(Theta)[i]),type='l',xaxt='n',yaxt='n')
}
dev.off()

# run sindy (using function in some_functions.R)
B = sindylicious(xs=xs,dx=dx)
B
# norm(Theta %*% XiD - dx)/norm(dx) # error


########################################################################
#
# Lorenz attractor
#
########################################################################

library(crqa)
dt = .001
numsteps = 50000; dt = dt; sigma = 10; r = 28; b = 2.6;
xs = data.frame(lorenzattractor(numsteps, dt, sigma, r, b, plots=F))
colnames(xs) = list('x','y','z')

xs = xs[2000:nrow(xs),] # cut out initialization
pdf('figures/figure_3_3d.pdf',width=4,height=4)
points3D(xs$x,xs$y,xs$z,type='l',col='black')
dev.off()

# plot the individual variables
lambda = .02
pdf('figures/figure_3_xyz.pdf',width=4,height=4)
par(mfrow=c(3,1),oma = c(0,0,0,0) + 0.1,mar = c(4.1,4.1,1,1) + 0.1)
plot(xs$x,type='l',xlab='t',ylab='x')
plot(xs$y,type='l',xlab='t',ylab='y')
plot(xs$z,type='l',xlab='t',ylab='z')
dev.off()

# plot the features
Theta = features(xs,3) # grid of features
par(mfrow=c(3,3),oma = c(2,0,0,0) + 0.1,mar = c(1,1,1,1) + 0.1)
for (i in 2:ncol(Theta)) {
  plot(Theta[,i],xlab='t',main=gsub(':','',colnames(Theta)[i]),type='l',xaxt='n',yaxt='n')
}
dev.off()

B = sindylicious(xs=xs,dt=dt,lambda=.5)
B

########################################################################
#
# two-well attractor behavior
#
########################################################################

# the model
pdf(file='figures/figure_4_traj.pdf',height=5,width=5)
x = seq(from=-2,to=2,by=.01)
V = (0*x - x^2/2 + x^4/4)
plot(x,V,xlab='x',type='l',lwd=2)
dev.off()

deets = c()
pdf(file='figures/figure_5_traj.pdf',height=5,width=5)
for (i in 1:100) {
  print(i)
  x_trial = c()
  x = 0
  k = runif(1)-.5
  x_sum = 0
  for (j in 1:1000) {
    #x_trial = rbind(x_trial,data.frame(t=j,x=x,x2=x^2,x3=x^3,k=k))
    x_trial = rbind(x_trial,data.frame(k=k,x=x,x_sum=x_sum))
    lx = x
    x = x + (-k + x - x^3) 
    x_sum = x_sum + x
    
    if (abs(x_sum)>10) { 
      if (i==1) {
        #par(mfrow=c(1,1))
        plot(x_trial$x_sum,(1:j)/j,type='l',xlim=c(-11,11),xlab='Cumulative x',ylab='Iteration (max scaled)')
      } else {
        points(x_trial$x_sum,(1:j)/j,type='l')
      }
      
      break; 
    }
    
  }
  deets = rbind(deets,x_trial)
}
dev.off()

B = sindylicious(xs=deets[1:(nrow(deets)-1),1:2],dx=as.matrix(deets[2:nrow(deets),1:2]),lambda=.6,dt=1);B 

pdf(file='figures/figure_4_traj.pdf',height=5,width=5)
x = seq(from=-2,to=2,by=.01)
V = (0*x - x^2/2 + x^4/4)
plot(x,V,xlab='x',type='l',lwd=2)
dev.off()

########################################################################
#
# two-well attractor behavior, with some noise... BOH!
#
########################################################################

# for GOF tests, here's a desired fit from the Tuller et al. model (Duran & Dale)
B_good = matrix(0,nrow=10,ncol=2)
B_good[2,1] = 1
B_good[2,2] = -1
B_good[3,2] = 2
B_good[10,2] = -1

rmses = c()
for (noise in seq(from=0,to=.25,length=1000)) {
  deets = c()
  print(noise)
  for (i in 1:100) {
    x_trial = c()
    x = 0
    k = runif(1)-.5
    x_sum = 0
    for (j in 1:1000) {
      #x_trial = rbind(x_trial,data.frame(t=j,x=x,x2=x^2,x3=x^3,k=k))
      x_trial = rbind(x_trial,data.frame(k=k,x=x,x_sum=x_sum))
      lx = x
      x = x + (-k + x - x^3) + noise*(rnorm(1))
      x_sum = x_sum + x
      
      if (abs(x_sum)>10) { 
        break; 
      }
      
    }
    deets = rbind(deets,x_trial)
  }
  dev.off()
  B = sindylicious(xs=deets[1:(nrow(deets)-1),1:2],dx=as.matrix(deets[2:nrow(deets),1:2]),lambda=.6,dt=1);
  rmses = rbind(rmses, data.frame(noise=noise,rmse=mean((B-B_good)^2)))
}
pdf(file='figures/figure_6_rmse_twowell.pdf',height=5,width=5)
plot(rmses$noise,rmses$rmse,type='b',xlab='Noise',ylab='RMSE')
dev.off()

########################################################################
#
# Sindy as descriptive variable -- two two well systems...
#
########################################################################

complexities = c()
noise = .2

for (iter in 1:100) {
  print(iter)
  deets = c()
  for (i in 1:100) {
    x_trial = c()
    x = 0
    k = runif(1)-.5
    x_sum = 0
    for (j in 1:1000) {
      #x_trial = rbind(x_trial,data.frame(t=j,x=x,x2=x^2,x3=x^3,k=k))
      x_trial = rbind(x_trial,data.frame(k=k,x=x,x_sum=x_sum))
      lx = x
      x = x + (-k + x - x^3) + noise*(rnorm(1))
      x_sum = x_sum + x
      
      if (abs(x_sum)>10) { 
        break; 
      }
    }
    deets = rbind(deets,x_trial)
  }
  B = sindylicious(xs=deets[1:(nrow(deets)-1),1:2],dx=as.matrix(deets[2:nrow(deets),1:2]),lambda=.6,dt=1);
  complexities = rbind(complexities, data.frame(iter=iter,noise=noise,expected=4,complexity=sum(B!=0)))
  
  deets = c()
  for (i in 1:100) {
    x_trial = c()
    x = 0
    k = runif(1)-.5
    x_sum = 0
    for (j in 1:1000) {
      #x_trial = rbind(x_trial,data.frame(t=j,x=x,x2=x^2,x3=x^3,k=k))
      x_trial = rbind(x_trial,data.frame(k=k,x=x,x_sum=x_sum))
      lx = x
      x = x + (-k + x + x^2 - x^3) + noise*(rnorm(1))
      x_sum = x_sum + x
      
      if (abs(x_sum)>10) { 
        break; 
      }
    }
    deets = rbind(deets,x_trial)
  }
  B = sindylicious(xs=deets[1:(nrow(deets)-1),1:2],dx=as.matrix(deets[2:nrow(deets),1:2]),lambda=.6,dt=1);
  complexities = rbind(complexities, data.frame(iter=iter,noise=noise,expected=5,complexity=sum(B!=0)))
}

summary(lm(complexity~as.factor(expected),data=complexities))

pdf(file='figures/figure_6_complexity_fit.pdf',height=5,width=5)
boxplot(jitter(complexity)~as.factor(expected),data=complexities,ylab='Coefficients (with jitter)',xlab='Expected coefficients')
dev.off()





