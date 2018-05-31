#' Main SINDy Code
#' 
#' @param xs matrix of raw data
#' @param dx matrix of main system variable dervatives; if NULL, sindy estimates with finite differences from xs
#' @param Theta matrix of features; if not supplied, assumes polynomial features of order 3
#' @param lambda threshold to use for iterated least squares sparsification (Brunton et al.)
#' @param B.expected 
#' @param verbose
#' @return a sindy object (S3 class) with coefficients (B), original data, some metrics, and so on; e.g., sindy$B

.packageName <- 'sindyr'

sindy = function(xs,dx=NULL,dt=1,Theta=NULL,lambda=.05, # main parameters
                 B.expected=NULL,verbose=F,fit.its=10,
                 plot.eq.graph=F, # wanna graph a network from the terms?
                 eq.graph.par=list(vertex.size=20,edge.arrow.size=.25,vertex.label.cex=1,layout=layout_nicely)) {
  
  if (is.null(dx)) { # if dx not supplied, let's estimate it
    dx = xs*0 # initialize to 0
    for (i in 1:ncol(xs)) {
      dx[,i] = finite_difference(xs[,i],dt) # take finite differences approach
    }
    dx = as.matrix(dx) # store as matrix
  }
  
  if (is.null(Theta)) {
    Theta = features(xs,polyorder=3) # if Theta not specified, make it (assume order 3)
  }
  
  B = mldivide(Theta,dx) # now, the party startsy; let's do left division
  if (lambda>0) { # if lambda is zero, then all our coefficients stay, otherwise...
    for (k in 1:fit.its) { # ...cycle 10 (fit.its) times and refit
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
  
  if (!is.null(B.expected)) {
    B.err = sqrt(mean((B.expected-B)^2))
  } else { B.err = NULL }
  
  p_dx = Theta %*% B # prediction error
  pred.err = sqrt(mean(p_dx[,2]-dx[,2])^2)
  
  simple.kolmog = sum(B!=0)
  prop.coef = sum(B!=0)/length(B)
  
  if (verbose) {
    sindy.obj = list(B=B,Theta=Theta,dx=dx,
                     lambda=lambda,pred.err=pred.err,
                     B.expected=B.expected,B.err=B.err,
                     simple.kolmog=simple.kolmog,prop.coef=prop.coef)
  } else {
    sindy.obj = list(B=B,lambda=lambda,
                     pred.err=pred.err,B.err=B.err,
                     simple.kolmog=simple.kolmog,
                     prop.coef=prop.coef)
  }
  
  if (plot.eq.graph) {
    B = sindy.obj$B
    main.vars = colnames(B)
    B = cbind(data.frame(0,B))
    ixs = which(B!=0,arr.ind=T)
    g = graph.data.frame(ixs,directed=T)
    nms = row.names(B)[unique(as.vector(ixs))]
    intersect(main.vars,nms)
    V(g)$color <- "white" 
    for (mv in main.vars) {
      V(g)$color[which(nms==mv)] <- "green"
    }
    plot(g,vertex.label=nms,
         vertex.size=eq.graph.par$vertex.size,edge.arrow.size=eq.graph.par$edge.arrow.size,
         vertex.label.cex=eq.graph.par$vertex.label.cex,layout=eq.graph.par$layout)
  }  
  
  return(sindy.obj) # put it in the mailbox
}

