#' Perform Method of Finite Differences Over Multiple Columns
#' 
#' @param xs raw data to be differentiated
#' @param S sample rate of data to return derivatives using raw time
#' @return set of first-order numerical derivatives estimated from data, using finDiff

.packageName <- 'sindyr'

finite_differences = function(xs,S) {
  dx = c()
  for (i in 1:ncol(xs)) { # if we have lots of state variables, let's findDiff 'em individually
    dx = cbind(dx,finite_difference(xs[,i],S))
  }
  return(dx) # Sorry dx, I have to ask you to leave. Know I'll miss you.
}
