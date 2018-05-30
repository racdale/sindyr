#########################################################################
#
# coded by Rick Dale
# Adapted from the MATLAB code of Brunton et al. (2016), PNAS
#
#########################################################################

setwd('~/Dropbox/new.projects/Dale and Bhat CogSys SINDy/sindyr/dale-bhat-materials/')

library(pracma)
library(entropy)
library(sindyr)

########################################################################
#
# Coupled logistic maps (from Buder, 1991; see Dale & Bhat for details)
#
########################################################################

as = seq(from=2.5,to=3,by=.05) # gather coupled map data
all_data = c()
x = runif(1)
c_y_to_x = 1 # strength of influence between system x and y
c_x_to_y = 1
for (a in as) {
  print(a)
  data_temp = c()
  x = runif(1)
  y = runif(1)
  for (i in 1:100) {
    # print(xs)
    # if (runif(1)>.5) {
       x = a*(1-x)*(1-c_y_to_x*(x-y))*x 
       y = a*(1-y)*(1-c_x_to_y*(y-x))*y
       data_temp = rbind(data_temp,data.frame(a=a,x=x,y=y))
    # } else {
    #   y = a*(1-y)*(1-c_x_to_y*(y-x))*y
    #   x = a*(1-x)*(1-c_y_to_x*(x-y))*x 
    # }
    # if (x>1) {x=1} # force back to 1; outside of unit interval leads to Inf
    # if (y>1) {y=1}
    
  }
  all_data = rbind(all_data,data_temp)
}  
all_data[1:4,]
hist(all_data[,2])

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

# run sindy (using function in some_functions.R)
# given y is t+1 relative to x, we must make a shift:
dx = as.matrix(all_data[2:(nrow(all_data)),])
xs = all_data[1:(nrow(all_data)-1),]
#B = sindyr::sindy(xs=xs,dx=dx,Theta=features(xs,4),lambda=0.3)
ts = c()
for (o in 2:7) {
  print(o)
  t1 = proc.time()
  sindy.obj = sindy(xs=xs,dx=dx,Theta=features(xs,3,intercept=T),lambda=.05)
  t2 = proc.time()
  ts = c(ts,t2[3]-t1[3])
}
plot(2:7,ts,type='b')
sindy.obj$B
cbind(sindy.obj$B,B.expected)
# norm(Theta %*% XiD - dx)/norm(dx) # error

#
# method for identifying best threshold
#

# expected coefficients for x
B.expected = matrix(0,nrow=35,ncol=3)
B.expected[5,2] = 1
B.expected[11,2] = 1
B.expected[13,2] = -2
B.expected[26,2] = 1
B.expected[22,2] = -1

# expected coefficients for y
B.expected[6,3] = 1
B.expected[11,3] = 1
B.expected[15,3] = -2
B.expected[29,3] = 1
B.expected[23,3] = -1

# and for a
B.expected[2,1] = 1

ers=c()
thresholds = seq(from=0,to=1,by=.05)

for (threshold in thresholds) {
  print(threshold)
  #sindy.obj = sindyr::sindy(xs=all_data,dx=dx,Theta=features(xs,4),lambda=threshold)
  sindy.obj = sindy(xs=all_data,dx=dx,Theta=features(xs,4),lambda=threshold,B.expected=B.expected)
  #ent = entropy(abs(B[,2]))/log(length(B[,2]))
  ers = rbind(ers,data.frame(threshold=threshold,
                         ground.truth.error=sindy.obj$B.err,
                         prediction.error=sindy.obj$pred.err,score=sindy.obj$prop.coef))
  
}
plot(ground.truth.error~threshold,data=ers,type='b',ylim=c(0,10),lwd=2,col='green')
#points(ent~threshold,data=ers,type='b',col='red',lwd=2)
points(prediction.error~threshold,data=ers,type='b',col='blue',lwd=2)
points(score~threshold,data=ers,type='b',col='orange',lwd=2)







