\name{finite_difference}
\alias{finite_difference}
\title{Estimate derivative of variable with finite differences}
\description{
  Estimates first-order derivatives of a vector }

\usage{

}

\arguments{
  \item{x}{Raw data to be differentiated}
  \item{S}{Sample rate of data to return derivatives using raw time}

}
\details{
  Uses simplest version of finite-difference method (window size 2) to
  numerically estimate derivative of a time series.
}

\value{Returns first-order numerical derivatives estimated from data.
}

\author{Rick Dale (racdale@gmail.com)}


\examples{
 
}

\keyword{misc}