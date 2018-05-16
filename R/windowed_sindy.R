#' Conduct SINDy Over Windows of Time Series 
#' 
#' @param xs matrix of raw data
#' @param dx matrix of main system variable dervatives; if NULL, sindy estimates with finite differences from xs
#' @param Theta matrix of features; if not supplied, assumes polynomial features of order 3
#' @param lambda threshold to use for iterated least squares sparsification (Brunton et al.)
#' @param window.size size of window to segment raw data as separate time series; defauls to deciles
#' @param window.shift step sizes across windows, permitting overlap; defaults to deciles
#' @return a list of coefficients Bs containing B coefficients at each window

.packageName <- 'sindyr'

windowed_sindy = function(xs,dx=NULL,dt=1,Theta=NULL,lambda=.05,
  window.size=round(length(xs)/10),window.shift=round(length(xs)/10)) {

}
