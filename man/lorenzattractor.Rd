\name{lorenzattractor}
\alias{lorenzattractor}

\title{Simulate the Lorenz Attractor}

\description{

  An implementation of the Lorenz dynamical system,
  which describes the motion of a possible particle, which will
  neither converge to a steady state, nor diverge to infinity;
  but rather stay in a bounded but 'chaotically' defined
  region, i.e., an attractor. 
  
}

\usage{
lorenzattractor(numsteps, dt, sigma, r, b)
}

\arguments{
  
  \item{numsteps}{The number of simulated points}
  \item{dt}{System parameter}
  \item{sigma}{System parameter}
  \item{r}{System parameter}
  \item{b}{System parameter}
  
}

\value{
  
  It returns a matrix with the 3 dimensions of the Lorenz
  
}
  
\references{

  Lorenz, Edward Norton (1963). Deterministic nonperiodic flow.
  Journal of the Atmospheric Sciences 20(2) 130-141.
  
}

\author{Moreno I. Coco (moreno.cocoi@gmail.com) }

\examples{

## initialize the parameters
numsteps = 2 ^ 11; dt = .01; sigma = 10; r = 28; b = 8/3;

res = lorenzattractor(numsteps, dt, sigma, r, b)

}

\keyword{ts}
