\name{sindyr-package}
\alias{sindyr-package}

\docType{package}

\title{
  Sparse Identification of Nonlinear Dynamics
}

\description{
    This implements the Brunton et al (2016; PNAS, doi: 10.1073/pnas.1517384113) sparse
    identification algorithm for finding ordinary differential equations for a measured 
    system from raw data (SINDy). The package includes a set of additional tools for 
    working with raw data, with an emphasis on cognitive science applications (Dale 
    and Bhat, 2018, doi: 10.1016/j.cogsys.2018.06.020). See 
    <https://github.com/racdale/sindyr> for examples and updates.
}
\details{
\tabular{ll}{
Package: \tab sindyr\cr
Type: \tab Package\cr
Version: \tab 0.2.1\cr
Date: \tab 2018-09-10\cr
License: \tab GPL >= 2\cr
}
 

\code{sindy}: Main function to infer coefficient matrix for set of ODEs.

\code{windowed_sindy}: Sliding window function to obtain SINDy results across segments of a time series.

\code{features}: Function for generation feature space from measured variables.

\code{finite_differences}: Numerical differentiation over multiple columns.

\code{finite_difference}: Numerical differential of a vector.

}

\author{Rick Dale and Harish S. Bhat}

\references{
  
  Dale, R. and Bhat, H. S. (2018). Equations of mind: data science for inferring nonlinear dynamics of socio-cognitive systems. Cognitive Systems Research, 52, 275-290.
  
  Brunton, S. L., Proctor, J. L., and Kutz, J. N. (2016). Discovering governing equations from data by sparse identification of nonlinear dynamical systems. Proceedings of the National Academy of Sciences, 113(15), 3932-3937.  

  For further examples and links to other materials see: https://github.com/racdale/sindyr
    
}

\keyword{ package }

\examples{

# example to reconstruct of 
# the Lorenz system

library(sindyr)

set.seed(666)
dt = .001
numsteps = 10000; dt = dt; sigma = 10; r = 28; b = 2.6;
xs = data.frame(lorenzattractor(numsteps, dt, sigma, r, b))
colnames(xs) = list('x','y','z')
xs = xs[2000:nrow(xs),] # cut out initialization

Theta = features(xs,3) # grid of features
par(mfrow=c(7,3),oma = c(2,0,0,0) + 0.1,mar = c(1,1,1,1) + 0.1)
for (i in 2:ncol(Theta)) {
  plot(Theta[,i],xlab='t',main=gsub(':','',colnames(Theta)[i]),type='l',xaxt='n',yaxt='n')
}

sindy.obj = sindy(xs=xs,dt=dt,lambda=.5) # let's reconstruct
sindy.obj$B # Lorenz equations

}