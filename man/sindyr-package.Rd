\name{sindyr-package}
\alias{sindyr-package}

\docType{package}

\title{
  Tools for Sparse Identification of Nonlinear Dynamics for R
}

\description{
  This implements the Brunton et al (2016; PNAS) sparse
    identification algorithm for finding ordinary differential
    equations (ODEs) for a measured system from raw data (SINDy). The library
    includes a set of additional tools for working with raw
    data, with an emphasis on cognitive science applications 
    (Dale and Bhat, in press).
}
\details{
\tabular{ll}{
Package: \tab sindyr\cr
Type: \tab Package\cr
Version: \tab 0.002\cr
Date: \tab 2018-07-04\cr
License: \tab GPL >= 2\cr
}
 

\code{sindy}: Main function to infer coefficient matrix for set of ODEs.

<<<<<<< HEAD
\code{windowed_sindy}: Sliding window function to obtain SINDy results across segments of a time series.

\code{features}: Function for generation feature space from measured variables.

\code{finite_differences}: Numerical differentiation over multiple columns.

=======
\code{sindy}: Main function to infer coefficient matrix for set of ODEs.

\code{windowed_sindy}: Sliding window function to obtain SINDy results across segments of a time series.

\code{features}: Function for generation feature space from measured variables.

\code{finite_differences}: Numerical differentiation over multiple columns.

>>>>>>> 36b5c64ffa301343e4a4bc0ae3155ccdd6be1bee
\code{finite_difference}: Numerical differential of a vector.

}

\author{Rick Dale and Harish S. Bhat}

\references{
  
  Dale, R. and Bhat, H. S. (in press). Equations of mind: data science for inferring
  nonlinear dynamics of socio-cognitive systems. Cognitive Systems Research.
  
  Brunton, S. L., Proctor, J. L., and Kutz, J. N. (2016). Discovering governing equations from data by sparse identification of nonlinear dynamical systems. Proceedings of the National Academy of Sciences, 113(15), 3932-3937.  
    
}

\keyword{ package }

\examples{

# example to reconstruct of 
# the Lorenz system

library(sindyr)
library(crqa) # for Lorenz

set.seed(666)
dt = .001
numsteps = 50000; dt = dt; sigma = 10; r = 28; b = 2.6;
xs = data.frame(lorenzattractor(numsteps, dt, sigma, r, b, plots=F))
colnames(xs) = list('x','y','z')
xs = xs[2000:nrow(xs),] # cut out initialization

points3D(xs$x,xs$y,xs$z,type='l',col='black')
Theta = features(xs,3) # grid of features
par(mfrow=c(7,3),oma = c(2,0,0,0) + 0.1,mar = c(1,1,1,1) + 0.1)
for (i in 2:ncol(Theta)) {
  plot(Theta[,i],xlab='t',main=gsub(':','',colnames(Theta)[i]),type='l',xaxt='n',yaxt='n')
}

sindy.obj = sindy(xs=xs,dt=dt,lambda=.5) # let's reconstruct
sindy.obj$B # Lorenz equations

}
