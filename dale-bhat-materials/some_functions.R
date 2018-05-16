#
# coded by Rick Dale and Harish Bhat
#

windowed_sindy = function(xs,dx=NULL,dt=1,Theta=NULL,lambda=.05,window.size=round(length(xs)/10),window.shift=round(length(xs)/10)) {
}

sindylicious = function(xs,dx=NULL,dt=1,Theta=NULL,lambda=.05) {
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

features = function(x,polyorder=2) {
  nc = ncol(x)
  if (polyorder>1) { # if polyorder is not greater than 1, then what are you doing here?
    for (i in 2:polyorder) { # let's make some snazzy names for our derived features
      x2 = x 
      colnames(x2) = paste(colnames(x),'_',i,sep='')
      x = cbind(x,x2) # make one for each order requested
    }
  }
  f = eval(parse(text=paste('~(.)^',polyorder,sep=''))) # now, let's get the full set of polyorder-th terms
  x_temp = model.matrix(f,x) # this will let us build the derived features
  x_nms = gsub('_\\d','',colnames(x_temp)) # we want to pass on the variable names here using _X as a search key
  uniq_nms = unique(x_nms) # this can produce some equivalences (e.g., xxy, yxx), so let's reduce
  for (i in 1:length(uniq_nms)) { # for all unique ones, let's get some nice names
    uniq_nms[i] = paste(sort(unlist(strsplit(uniq_nms[i],':'))),collapse=':') # rebuild interaction term names with :
  }
  uniq_nms = unique(uniq_nms) # let's reduce again looking for repeats
  x = c()
  for (n in uniq_nms) { # okay, let's start to build the materials for the party, based on what unique items remain
    x = cbind(x,x_temp[,which(x_nms==n)[1]])
  }
  colnames(x) = uniq_nms # now we have the unique names
  return(x) # press the red button and run
}

# S = unit of time across samples
finite_difference = function(x, S) {
  n = length(x)
  fdx <- vector(length = n)
  fdx[1] = (x[2]-x[1])/S # first derivative
  for (i in 3:(n-1)) {
    fdx[i-2] <- (x[i] - x[i-2]) / (2*S) # the intermediate ones
  }
  fdx[n] <- (x[n] - x[n - 1]) / S # the last one
  return(fdx) # exchange envelopes
}

finite_differences = function(xs,S) {
  dx = c()
  for (i in 1:ncol(xs)) { # if we have lots of state variables, let's findDiff 'em individually
    dx = cbind(dx,finDiff(xs[,i],S))
  }
  return(dx) # Sorry dx, I have to ask you to leave. Know I'll miss you.
}

