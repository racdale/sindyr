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
# Lorenz attractor
#
########################################################################

library(crqa) # for Lorenz
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
Theta = sindyr::features(xs,3) # grid of features
par(mfrow=c(3,3),oma = c(2,0,0,0) + 0.1,mar = c(1,1,1,1) + 0.1)
for (i in 2:ncol(Theta)) {
  plot(Theta[,i],xlab='t',main=gsub(':','',colnames(Theta)[i]),type='l',xaxt='n',yaxt='n')
}
dev.off()

B = sindyr::sindy(xs=xs,dt=dt,lambda=.5)
B
