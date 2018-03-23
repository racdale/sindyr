library(ggplot2)

setwd('/Users/rickdale/Dropbox/new.projects/multiModal/Dale and Bhat/R_code/')

eye.names = read.csv('tolston_data/Tolston_et_al_2014_JEPHPP/EYE_DATA_FORMAT.csv')
head.names = read.csv('tolston_data/Tolston_et_al_2014_JEPHPP/HEAD_DATA_FORMAT.csv')
waist.names = read.csv('tolston_data/Tolston_et_al_2014_JEPHPP/WAIST_HAND_DATA_FORMAT.csv')

pair = '08'
task = 'RR'
trial = '01'

pairs = c('01','02',paste('0',4:9,sep=''),paste(10:18,sep=''))
trials = c('01','02','03')
tasks = c('FF','FR','RR')

fits = c()
coefs = list()
coefs_i=1
lambda = .0005

for (lambda in seq(from=.005,to=.05,by=.01)) {
  for (pair in pairs) {
    for (trial in trials) { 
      for (task in tasks) {
        
        print(paste(pair,trial,task,lambda))
        
        rt = paste('tolston_data/Tolston_et_al_2014_JEPHPP/PAIR_',pair,'/FFF/',sep='')
        base = paste(rt,pair,'XXX',task,trial,'.csv',sep='')
        eye.file = gsub('FFF','EYE_DATA',gsub('XXX','E',base))
        head.file = gsub('FFF','HEAD_DATA',gsub('XXX','H',base))
        waist.file = gsub('FFF','WAIST_DATA',gsub('XXX','',base))
        
        dat.eyes = read.csv(eye.file)
        colnames(dat.eyes) = names(eye.names)
        dat.head = read.csv(head.file)
        colnames(dat.head) = names(head.names)
        dat.waist = read.csv(waist.file)
        colnames(dat.waist) = names(waist.names)
        
        data = cbind(p1_eyes_hgl=dat.eyes$P1.HORIZONTAL.GAZE.LOCATION,
                     p1_eyes_vgl=dat.eyes$P1.VERTICAL..GAZE.LOCATION,
                     p1_head_ap=dat.head$P1.HEAD.AP,
                     p1_head_ml=dat.head$P1.HEAD.ML,
                     p2_eyes_hgl=dat.eyes$P2.HORIZONTAL.GAZE.LOCATION,
                     p2_eyes_vgl=dat.eyes$P2.VERTICAL..GAZE.LOCATION,
                     p2_head_ap=dat.head$P2.HEAD.AP,
                     p2_head_ml=dat.head$P2.HEAD.ML)
        
        data = data[!is.na(rowSums(data)),]
        
        if (nrow(data)>3600) {
          
          data = data[600:(nrow(data)-600),]
          data = scale(data)
          data = data[!rowSums(abs(data)>4),]
          data = scale(data)
          
          # TRAIN
          xs = data[1:floor(nrow(data)/10),]
          Theta = features(data.frame(xs),2)
          dx = finDiffs(xs,1)
          Xi = sindylicious(xs,dx,Theta,lambda=lambda) # TRAIN
          
          train_fits = as.vector(Theta %*% Xi)
          train_observed = as.vector(dx)
          train_cor = cor(train_fits,train_observed)
          
          # TEST
          xs = data[ceil(nrow(data)/10):floor(2*nrow(data)/10),]
          Theta = features(data.frame(xs),2)
          dx = finDiffs(xs,1)
          test_fits = as.vector(Theta %*% Xi)
          test_observed = as.vector(dx)
          test_cor = cor(test_fits,test_observed)
          
          fits = rbind(fits,data.frame(pair=pair,task=task,trial=trial,
                                       train_r=train_cor,
                                       test_r=test_cor,
                                       l=lambda,
                                       n=sum(Xi>0)))
          
          coefs[[coefs_i]]=list(Xi,pair,task,trial,lamda,train_cor,test_cor,sum(Xi>0))
          coefs_i = coefs_i+1
          
        }  
        
      }}}} # for loops through subjects
save(file='output/fits_tenths.Rd',fits)
save(file='output/coefs_tenths.Rd',coefs)

load('output/fits_tenths.Rd')

pdf(file='output/data_aggregate_tenths.pdf',height=6,width=11)
par(mfrow=c(1,2))
l_maxes = c()
i=0
deets = coefs[[1]][[1]]*0
fits$l_0 = NA
for (pair in pairs) {
  for (trial in trials) { 
    for (task in tasks) {
      f = fits[fits$pair==pair&fits$task==task&fits$trial==trial,]      
      # plot(f$l,f$test_r,type='b')      
      l_maxes = c(l_maxes,f$l[which.max(f$test_r)[1]])
      l_max = f$l[which.max(f$test_r)[1]]
      f$l = f$l - l_max
      #f$test_r = f$test_r - min(f$test_r)
      #f$test_r = f$test_r/max(f$test_r)
      fits[fits$pair==pair&fits$task==task&fits$trial==trial,]$l_0 = 
        fits[fits$pair==pair&fits$task==task&fits$trial==trial,]$l - l_max
      if (i==0) {
        plot(f$l,f$test_r,type='p',xlim=c(-.08,.08),
             ylim=c(-1,1),col=rgb(0,0,0,.25),
             xlab='Best threshold = 0',ylab='Test set r')
      } else {
        points(f$l,f$test_r,type='p',col=rgb(0,0,0,.25))
      }
      # print(paste(dim(f)[1],pair,trial,task,f$l[which.max(f$test_r)[1]]))
      i = i + 1
      deets = deets + (coefs[[i]][[1]])
    }}}

fits$l_sq = fits$l_0^2
lmo = lm(test_r~l_sq,data=fits)
rg = seq(-.1,.1,by=.01)
points(rg,predict(lmo,data.frame(l_sq=rg^2)),type='l',lwd=3)

hist(l_maxes,xlab='Best threshold',main='')
dev.off()

par(mfrow=c(1,1))
# histogram says .02 or so is the best fitting
deets[deets<.02] = 0
filled.contour(t(deets))









# dt = 1
# 
# cs = c()
# for (lambda in seq(from=.001,to=.1,by=.001)) {
#   print(lambda)
#   
#   xs = raw.data[3000:4000,]
#   Theta = features(data.frame(xs),2)
#   dx = c()
#   for (i in 1:ncol(xs)) {
#     dx = cbind(dx,finDiff(xs[,i],dt))
#   }
#   
#   Xi = sindylicious(xs,dx,Theta,lambda=lambda)
#   #norm(Theta %*% XiD - dx)/norm(dx)
#   
#   xs = raw.data[4000:5000,]
#   Theta = features(data.frame(xs),2)
#   dx = c()
#   for (i in 1:ncol(xs)) {
#     dx = cbind(dx,finDiff(xs[,i],dt))
#   }
#   
#   fits = as.vector(Theta %*% Xi)
#   observed = as.vector(dx)
#   observed = observed[fits!=0]
#   fits = fits[fits!=0]
#   cs = rbind(cs,data.frame(
#     r=cor(fits,observed),
#     l=lambda,
#     n=sum(Xi>0)))
# }
# plot(cs$l,cs$r,type='b')
# plot(cs$n,cs$r,type='b')
# #plot(Theta %*% Xi,dx)
# 
# 
