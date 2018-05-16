\name{windowed_sindy}
\alias{windowed_sindy}
\title{Run SINDy over time windows}
\description{
  Run SINDy on raw data with a sliding window approach
}

\usage{

}

\arguments{
  \item{xs}{Matrix of raw data}
  \item{dx}{Matrix of main system variable dervatives; if NULL, it estimates with finite differences from xs}
  \item{Theta}{Matrix of features; if not supplied, assumes polynomial features of order 3}
  \item{lambda}{Threshold to use for iterated least squares sparsification (Brunton et al.)}
  \item{window.size}{Size of window to segment raw data as separate time series; defauls to deciles}
  \item{window.shift}{Step sizes across windows, permitting overlap; defaults to deciles}

}

\details{
	A convenience function for extracting a list of coefficients on
	segments of a time series. This facilitate susing SINDy output as
	source of descriptive measures of dynamics.
}

\value{It returns a list of coefficients Bs containing B coefficients at each window
}

\author{Rick Dale (racdale@gmail.com)}


\examples{
}

\keyword{misc}
