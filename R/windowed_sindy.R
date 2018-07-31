#' Conduct SINDy Over Windows of Time Series 
#' 
#' @param xs matrix of raw data
#' @param dx matrix of main system variable dervatives; if NULL, sindy estimates with finite differences from xs
#' @param dt sample interval, if data continuously sampled; default = 1
#' @param Theta matrix of features; if not supplied, assumes polynomial features of order 3
#' @param lambda threshold to use for iterated least squares sparsification (Brunton et al.)
#' @param fit.its number of iterations to conduct the least-square threshold sparsification; default = 10
#' @param B.expected the function will compute a goodness of fit if supplied with an expected coefficient matrix B; default = NULL
#' @param window.size size of window to segment raw data as separate time series; defauls to deciles
#' @param window.shift step sizes across windows, permitting overlap; defaults to deciles
#' @return a data frame of various statistics including window, B error, prediction error, model complexity

.packageName <- 'sindyr'

windowed_sindy = function(xs,dx=NULL,dt=1,Theta=NULL,lambda=.05,fit.its=10,B.expected=NULL,
                          window.size=round(nrow(xs)/10),window.shift=round(nrow(xs)/10)) {
  j = 1
  windowed.results = c()
  ixes = seq(from=1,to=nrow(xs)-window.size+1,by=window.shift)
  for (i in ixes) {
    print(paste0('Running window ',which(i==ixes),' of ',length(ixes)))
    sub.xs = xs[i:(i+window.size-1),]
    if (!is.null(dx)) {
      sub.dx = as.matrix(dx[i:(i+window.size-1),])
    } else {
      sub.dx = NULL
    }    
    sindy.obj = sindy(sub.xs,dx=sub.dx,dt=dt,Theta=Theta,lambda=lambda,B.expected=B.expected,verbose=F,fit.its=fit.its,plot.eq.graph=F)
    j = j + 1
    windowed.results = rbind(windowed.results,data.frame(t=i,window=j,window.size=window.size,
                                                         B.err=ifelse(is.null(sindy.obj$B.err),NA,sindy.obj$B.err),
                                                         pred.err=sindy.obj$pred.err,
                                                         simple.kolmog=sindy.obj$simple.kolmog,
                                                         prop.coef=sindy.obj$prop.coef))
  }
  return(windowed.results)
}
