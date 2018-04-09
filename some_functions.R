#
# coded by Rick Dale and Harish Bhat
#

sindylicious = function(xs,dx=NULL,dt=1,Theta=NULL,lambda=.05) {
  if (is.null(dx)) {
    dx = xs*0
    for (i in 1:ncol(xs)) {
      dx[,i] = finDiff(xs[,i],dt)
    }
    dx = as.matrix(dx)
  }
  if (is.null(Theta)) {
    Theta = features(xs,3)
  }
  B = mldivide(Theta,dx)
  if (lambda>0) {
    for (k in 1:10) {
      zero_inds = which(abs(B)<lambda);  
      B[zero_inds]=0;
      for (ind in 1:ncol(dx)) {
        B_temp = B[,ind]
        pos_inds = which(abs(B_temp)>=lambda)
        if (length(pos_inds)>0) {
          B[pos_inds,ind] = mldivide(Theta[,pos_inds],dx[,ind])
        }
      }
    }
  }
  rownames(B) = colnames(Theta)
  colnames(B) = colnames(xs)
  return(B)
}

features = function(x,polyorder=2) {
  nc = ncol(x)
  if (polyorder>1) {
    for (i in 2:polyorder) {
      x2 = x
      colnames(x2) = paste(colnames(x),'_',i,sep='')
      x = cbind(x,x2)
    }
  }
  f = eval(parse(text=paste('~(.)^',polyorder,sep='')))
  x_temp = model.matrix(f,x)
  x_nms = gsub('_\\d','',colnames(x_temp))
  uniq_nms = unique(x_nms)
  for (i in 1:length(uniq_nms)) {
    uniq_nms[i] = paste(sort(unlist(strsplit(uniq_nms[i],':'))),collapse=':')
  }
  uniq_nms = unique(uniq_nms)
  x = c()
  for (n in uniq_nms) {
    x = cbind(x,x_temp[,which(x_nms==n)[1]])
  }
  colnames(x) = uniq_nms
  return(x)
}

# S = unit of time across samples
finDiff = function(x, S) {
  n = length(x)
  fdx <- vector(length = n)
  fdx[1] = (x[2]-x[1])/S
  for (i in 3:(n-1)) {
    fdx[i-2] <- (x[i] - x[i-2]) / (2*S)
  }
  fdx[n] <- (x[n] - x[n - 1]) / S
  return(fdx)
}

finDiffs = function(xs,S) {
  dx = c()
  for (i in 1:ncol(xs)) {
    dx = cbind(dx,finDiff(xs[,i],S))
  }
  return(dx)
}

