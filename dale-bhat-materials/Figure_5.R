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
# two-well attractor behavior
#
########################################################################

# the model
pdf(file='figures/figure_5_traj.pdf',height=5,width=5)
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

B = sindyr::sindy(xs=deets[1:(nrow(deets)-1),1:2],dx=as.matrix(deets[2:nrow(deets),1:2]),lambda=.6,dt=1);
B 

