\name{finite_differences}
\alias{finite_differences}
\title{Estimate derivatives of multiple variables with finite differences}
\description{
  Estimates first-order derivatives of a column vectors of matrix }

\usage{

}

\arguments{
  \item{xs}{Raw data to be differentiated (matrix)}
  \item{S}{Sample rate of data to return derivatives using raw time}

}
\details{
  Uses simplest version of finite-difference method (window size 2) to
  numerically estimate derivative of multiple columnar time series.
}

\value{Returns first-order numerical derivatives estimated from data.
}

\author{Rick Dale (racdale@gmail.com)}


\examples{
 
}

\keyword{misc}