\name{windowed_sindy}
\alias{windowed_sindy}
\title{Run SINDy over time windows}
\description{
  Run SINDy on raw data with a sliding window approach
}

\arguments{
  \item{xs}{Matrix of raw data}
  \item{dx}{Matrix of main system variable dervatives; if NULL, it estimates with finite differences from xs}
  \item{dt}{Sample interval, if data continuously sampled; default = 1}  
  \item{Theta}{Matrix of features; if not supplied, assumes polynomial features of order 3}
  \item{lambda}{Threshold to use for iterated least squares sparsification (Brunton et al.)}
  \item{fit.its}{Number of iterations to conduct the least-square threshold sparsification; default = 10}
  \item{B.expected}{The function will compute a goodness of fit if supplied with an expected coefficient matrix B; default = NULL}
  \item{window.size}{Size of window to segment raw data as separate time series; defaults to deciles}
  \item{window.shift}{Step sizes across windows, permitting overlap; defaults to deciles}
}

\details{
	A convenience function for extracting a list of coefficients on
	segments of a time series. This facilitates using SINDy output as
	source of descriptive measures of dynamics. 
}

\value{It returns a list of coefficients Bs containing B coefficients at each window
}

\author{Rick Dale and Harish S. Bhat}

\references{
  
  Dale, R. and Bhat, H. S. (in press). Equations of mind: data science for inferring
  nonlinear dynamics of socio-cognitive systems. Cognitive Systems Research.
  
}

\keyword{misc}
