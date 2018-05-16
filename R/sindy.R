#' Main SINDy Code
#' 
#' @param xs matrix of raw data
#' @param dx matrix of main system variable dervatives; if NULL, sindy estimates with finite differences from xs
#' @param Theta matrix of features; if not supplied, assumes polynomial features of order 3
#' @param lambda threshold to use for iterated least squares sparsification (Brunton et al.)
#' @return a matrix B of coefficients specifying the relationship between dx and Theta

.packageName <- 'sindyr'

sindy = function(xs,dx=NULL,dt=1,Theta=NULL,lambda=.05) {
  if (is.null(dx)) { # if dx not supplied, let's estimate it
    dx = xs*0 # initialize to 0
    for (i in 1:ncol(xs)) {
      dx[,i] = finDiff(xs[,i],dt) # take finite differences approach
    }
    dx = as.matrix(dx) # store as matrix
  }
  if (is.null(Theta)) {
    Theta = features(xs,polyorder=3) # if Theta not specified, make it (assume order 3)
  }
  B = mldivide(Theta,dx) # now, the party startsy; let's do left division
  if (lambda>0) { # if lambda is zero, then all our coefficients stay, otherwise...
    for (k in 1:10) { # ...cycle 10 times and refit
      zero_inds = which(abs(B)<lambda); # find which should be zero
      B[zero_inds]=0; # set 'em to zero
      for (ind in 1:ncol(dx)) { # go through our system variables
        B_temp = B[,ind] 
        pos_inds = which(abs(B_temp)>=lambda) # which are positive indices?
        if (length(pos_inds)>0) { # now, let's refit B with just those remaining indices
          B[pos_inds,ind] = mldivide(Theta[,pos_inds],dx[,ind])
        }
      }
    }
  }
  rownames(B) = colnames(Theta) # let's make sure we can recognize B after all this
  colnames(B) = colnames(xs) # so that B feels like a warm, familiar friend
  return(B) # put it in the mailbox
}