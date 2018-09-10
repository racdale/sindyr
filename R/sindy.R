#' Main SINDy Code
#' 
#' @param xs matrix of raw data
#' @param dx matrix of main system variable dervatives; if NULL, sindy estimates with finite differences from xs
#' @param Theta matrix of features; if not supplied, assumes polynomial features of order 3
#' @param lambda threshold to use for iterated least squares sparsification (Brunton et al.)
#' @param B.expected the function will compute a goodness of fit if supplied with an expected coefficient matrix B; default = NULL
#' @param verbose verbose mode outputs Theta and dx values to the SINDy object... (S3, see below); default FALSE
#' @param fit.its number of iterations to conduct the least-square threshold sparsification; default = 10
#' @param plot.eq.graph plot an igraph network of the terms based on coefficients of SINDy model (default: FALSE)
#' @return a sindy object (S3 class) with coefficients (B), original data, some metrics, and so on; e.g., sindy$B

.packageName <- 'sindyr'

sindy = function(xs,dx=NULL,dt=1,Theta=NULL,lambda=.05, # main parameters
                 B.expected=NULL,verbose=FALSE,fit.its=10,
                 plot.eq.graph=FALSE) {
  
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
  colnames(B) = colnames(xs) 
  
  if (!is.null(B.expected)) {
    B.err = sqrt(mean((B.expected-B)^2))
  } else { B.err = NULL }
  
  p_dx = Theta %*% B # prediction error
  pred.err = sqrt(mean(p_dx-dx)^2)
  
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
    intersect(main.vars,nms) # get the main variables (columns)
    
    # let's color the core variables vs. predictors (white)
    V(g)$color = "white" 
    for (mv in main.vars) {
      V(g)$color[which(nms==mv)] = "green"
    }
    
    # we want to size the rectangles by variable name... 
    V(g)$label = nms
    co = layout_nicely(g)
    plot(0, type="n", ann=FALSE, axes=FALSE, xlim=extendrange(co[,1]), 
         ylim=extendrange(co[,2]))
    plot(g,rescale=F,add=T,
         edge.arrow.size=1,
         vertex.label.cex=1.5,
         layout=co,
         vertex.shape="rectangle",
         vertex.size=(strwidth(V(g)$label) + strwidth("oo")) * 100,
         vertex.size2=strheight("I")*2*100,asp=0)
  }  
  
  return(sindy.obj) # put it in the mailbox
}

