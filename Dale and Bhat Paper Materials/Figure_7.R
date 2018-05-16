#########################################################################
#
# coded by Rick Dale and Harish Bhat
# Adapted from the MATLAB code of Brunton et al. (2016), PNAS
#
#########################################################################
setwd('~/Dropbox/new.projects/Dale and Bhat CogSys SINDy/')
source('R_code/some_functions.R')
library(pracma)
library(entropy)
########################################################################
#
# Coupled logistic maps
#
########################################################################

as = seq(from=2.3,to=3,by=.1)
all_data = c()
x = runif(1)
c_y_to_x = 1
c_x_to_y = 1
for (a in as) {
  print(a)
  data_temp = c()
  x = runif(1)
  y = runif(1)
  for (i in 1:200) {
    #print(xs)
    y = a*(1-y)*(1-c_x_to_y*(y-x))*y
    x = a*(1-x)*(1-c_y_to_x*(x-y))*x
    data_temp = rbind(data_temp,data.frame(a=a,x=x,y=y))
  }
  all_data = rbind(all_data,data_temp)
}  
all_data[1:4,]
hist(all_data[,2])

# 
# let's get some plots
#

pdf('figures/figure_7_a.pdf',width=5,height=5)
plot(xs[,1],type='p',cex=.5,pch=15,xlab='t',ylab='a') # a
dev.off()
pdf('figures/figure_7_dx.pdf',width=5,height=5)
plot(dx,type='p',cex=.5,pch=15,xlab='t',ylab='dx') # dx
dev.off() 
pdf('figures/figure_7_x.pdf',width=5,height=5)
plot(xs[,2],type='p',cex=.5,pch=15,xlab='t',ylab='x') # x
dev.off()
pdf('figures/figure_7_features.pdf',width=4,height=4)
Theta = features(xs,3) # grid of features
par(mfrow=c(3,3),oma = c(2,0,0,0) + 0.1,mar = c(1,1,1,1) + 0.1)
for (i in 2:ncol(Theta)) {
  plot(Theta[,i],xlab='t',main=gsub(':','',colnames(Theta)[i]),type='l',xaxt='n',yaxt='n')
}
dev.off()

# run sindy (using function in some_functions.R)
dx = as.matrix(all_data[2:nrow(all_data),])
xs = all_data[1:(nrow(all_data)-1),]
B = sindylicious(xs=all_data,dx=dx,Theta=features(xs,4),lambda=.4)
B
# norm(Theta %*% XiD - dx)/norm(dx) # error

#
# method for identifying best threshold?
# idea: it is somewhere between sparsity, range of paragraph (integer?)
#

# for x
B_good = matrix(0,nrow=35,ncol=3)
B_good[5,2] = 1
B_good[11,2] = 1
B_good[13,2] = -2
B_good[26,2] = 1
B_good[22,2] = -1

# for y
B_good = matrix(0,nrow=35,ncol=3)
B_good[5,2] = 1
B_good[11,2] = 1
B_good[13,2] = -2
B_good[26,2] = 1
B_good[22,2] = -1

# first thing to do: check B_good for y, and determine the needed threshold... it is not obvious

ers=c()
thresholds = seq(from=0,to=2,by=.2)
for (threshold in thresholds) {
  print(threshold)
  B = sindylicious(xs=all_data,dx=dx,Theta=features(xs,4),lambda=threshold)
  ent = entropy(abs(B[,2]))/log(length(B[,2]))
  score = sum(B[,2]>0)/length(B[,2])
  p_dx = features(xs,4) %*% B
  prediction.error = sqrt(mean(p_dx[,2]-dx[,2])^2)
  ers = rbind(ers,data.frame(threshold=threshold,
                         ground.truth.error=sqrt(mean((B_good[,2]-B[,2])^2)),
                         ent=ent,prediction.error=prediction.error,score=score))
  
}
plot(ground.truth.error~threshold,data=ers,type='b',ylim=c(0,10),lwd=2,col='green')
points(ent~threshold,data=ers,type='b',col='red',lwd=2)
points(prediction.error~threshold,data=ers,type='b',col='blue',lwd=2)
points(score~threshold,data=ers,type='b',col='orange',lwd=2)



