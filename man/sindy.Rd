\name{sindy}
\alias{sindy}

\title{Run main SINDy function}
\description{
  Estimates coefficients for set of ordinary differential equations
  governing system variables.
  }

\arguments{
  \item{xs}{Matrix of raw data}
  \item{dx}{Matrix of main system variable dervatives; if NULL, it estimates with finite differences from xs}
  \item{dt}{Sample interval, if data continuously sampled; default = 1}    
  \item{Theta}{Matrix of features; if not supplied, assumes polynomial features of order 3}
  \item{lambda}{Threshold to use for iterated least squares sparsification (Brunton et al.)}
  \item{B.expected}{The function will compute a goodness of fit if supplied with an expected coefficient matrix B; default = NULL}
  \item{verbose}{Verbose mode outputs Theta and dx values in their entirety; default = FALSE}
  \item{fit.its}{Number of iterations to conduct the least-square threshold sparsification; default = 10}
  \item{plot.eq.graph}{When set to TRUE, prints an igraph plot of variables as a graph structure; default = FALSE}
}
\details{
 Uses the "left-division" approach of Brunton et al. (2016), and implements least-squares sparsification, and outputs coefficients after iterations stabilize.
}

\value{Returns a matrix B of coefficients specifying the relationship between dx and Theta
}

\author{Rick Dale and Harish S. Bhat}

\references{
  
  Dale, R. and Bhat, H. S. (in press). Equations of mind: data science for inferring
  nonlinear dynamics of socio-cognitive systems. Cognitive Systems Research.

  Brunton, S. L., Proctor, J. L., and Kutz, J. N. (2016). Discovering governing equations from data by sparse identification of nonlinear dynamical systems. Proceedings of the National Academy of Sciences, 113(15), 3932-3937.  
  
}


\keyword{misc}
