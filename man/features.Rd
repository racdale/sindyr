\name{features}
\alias{features}
\title{Build a matrix of features for SINDy}
\description{
  Takes a raw matrix of data and converts into polynomial features
  }


\arguments{
  \item{x}{Raw data to be converted into features}
  \item{polyorder}{Order of polynomials (including k-th self products)} 
  \item{intercept}{Include column of 1s in features to represent intercept (default = TRUE)}
}
\details{
 Expands raw data into a set of polynomial features.
}

\value{Returns a new matrix of data with features from raw data
}

\author{Rick Dale and Harish S. Bhat}


\keyword{misc}
