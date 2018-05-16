#' Generate a sequence of numeric identifiers
#' 
#' @param RP recurrence matrix, output from crqa library
#' @param xlab x label on plot's x-axis
#' @param ylab y label on plot's y-axis
#' @param cex size of marker
#' @return generates a plot; no value returned

.packageName <- 'crqanlp'

## should not we pass the par() arguments directly from the plot function?

plot_rp = function(RP,xlab='i',ylab='j',cex=.1) { ## should not this contain also a filepath to store the RP?
  if (!is.matrix(RP)) { RP = as.matrix(RP) }
  ij = which(RP==1,arr.ind=T)
  plot(ij[,1],ij[,2],cex=cex,xlab=xlab,ylab=ylab,pch=16) # should not pch also be parametrizable?
}
