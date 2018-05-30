#########################################################################
#
# coded by Rick Dale
# Adapted from the MATLAB code of Brunton et al. (2016), PNAS
#
#########################################################################

setwd('~/Dropbox/new.projects/Dale and Bhat CogSys SINDy/sindyr/dale-bhat-materials/')

library(pracma)
library(sindyr)

########################################################################
#
# Coupled logistic maps (from Buder, 1991; see Dale & Bhat for details)
#
########################################################################

coupled_logistic = function(seed=69,connection=1,iterations_per_a=100) {
  set.seed(seed)
  as = seq(from=2.4,to=4-connection,by=.01) # gather coupled map data
  # note that control parameter is dictated by connection -- 
  # fully connected, limit is 3, not at all, 4 (standard logistic)
  all_data = c()
  x = runif(1)
  y = runif(1)
  c_y_to_x = connection # strength of influence between system x and y
  c_x_to_y = connection
  for (a in as) {
    data_temp = c()
    for (i in 1:iterations_per_a) {
      x = a*(1-x)*(1-c_y_to_x*(x-y))*x 
      y = a*(1-y)*(1-c_x_to_y*(y-x))*y
      data_temp = rbind(data_temp,data.frame(a=a,x=x,y=y))
      if (x>1) { x = 1 }
      if (y>1) { y = 1 }
    }
    all_data = rbind(all_data,data_temp)
  }  
  return(all_data)
}

connections = seq(from=0,to=1,by=.05)
for (connection in connections) {
  xs = coupled_logistic(seed=67,connection=connection,iterations_per_a=100)
  dx = as.matrix(xs[2:(nrow(xs)),])
  xs = xs[1:(nrow(xs)-1),]
  pdf(paste0('figures/figure_7_nets/',connection,'.pdf'),width=5,height=3)
  sindy.obj = sindy(xs=xs,dx=dx,Theta=features(xs,3),lambda=.4,fit.its=10,plot.eq.graph=T)
  dev.off()
  sindy.obj$B
}

# 
# plot of system variables
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

