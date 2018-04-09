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
