#########################################################################
#
# coded by Rick Dale
# Adapted from the MATLAB code of Brunton et al. (2016), PNAS
#
#########################################################################

setwd('~/Dropbox/new.projects/multiModal/Dale and Bhat/')
source('R_code/some_functions.R')
library(pracma)
library(sindyr)

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
  B = sindyr::sindy(xs=deets[1:(nrow(deets)-1),1:2],dx=as.matrix(deets[2:nrow(deets),1:2]),lambda=.6,dt=1);
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
  B = sindyr::sindy(xs=deets[1:(nrow(deets)-1),1:2],dx=as.matrix(deets[2:nrow(deets),1:2]),lambda=.6,dt=1);
  complexities = rbind(complexities, data.frame(iter=iter,noise=noise,expected=5,complexity=sum(B!=0)))
}

summary(lm(complexity~as.factor(expected),data=complexities))

pdf(file='figures/figure_6_complexity_fit.pdf',height=5,width=5)
boxplot(jitter(complexity)~as.factor(expected),data=complexities,ylab='Coefficients (with jitter)',xlab='Expected coefficients')
dev.off()