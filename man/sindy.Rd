\name{sindyr}
\alias{sindyr}

\title{Run main SINDy function}
\description{
  Estimates coefficients for set of ordinary differential equations
  governing system variables.
  }

\usage{

}

\arguments{
  \item{xs}{Matrix of raw data}
  \item{dx}{Matrix of main system variable dervatives; if NULL, it estimates with finite differences from xs}
  \item{Theta}{Matrix of features; if not supplied, assumes polynomial features of order 3}
  \item{lambda}{Threshold to use for iterated least squares sparsification (Brunton et al.)}
}
\details{
 Uses the "left-division" approach of Brunton et al. (2016), and implements least-squares sparsification, and outputs coefficients after iterations stabilize.
}

\value{Returns a matrix B of coefficients specifying the relationship between dx and Theta
}

\author{Rick Dale (racdale@gmail.com)}


\examples{

}

\keyword{misc}
