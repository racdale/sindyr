#' Perform Method of Finite Differences Over One Column
#' 
#' @param x raw data to be differentiated
#' @param S sample rate of data to return derivatives using raw time
#' @return first-order numerical derivatives estimated from data

.packageName <- 'sindyr'

# S = unit of time across samples
finite_difference = function(x, S) {
  n = length(x)
  fdx <- vector(length = n)
  fdx[1] = (x[2]-x[1]) / S # first derivative
  for (i in 3:n) {
    fdx[i-1] = (x[i] - x[i-2]) / (2 * S) # the intermediate ones
  }
  fdx[n] = (x[n] - x[n - 1]) / S # the last one
  return(fdx) # exchange envelopes
}
