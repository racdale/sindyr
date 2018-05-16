\name{crqanlp-package}
\alias{crqanlp-package}

\docType{package}

\title{
  Dynamic Natural Language Processing with 
  Recurrence Quantification Analysis
}

\description{
  crqanlp is a package for conducting dynamic analysis of text under   
  recurrence quantification, which builts upon the `crqa` package.
}
\details{
\tabular{ll}{
Package: \tab crqanlp\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2017-03-05\cr
License: \tab GPL >= 2\cr
}


\code{assign_codes}:

\code{clean_text}: 

\code{get_text_series}: 

\code{plot_rp}: 

\code{text_rqa}: 

\code{text_win_rqa}: 


}

\author{Rick Dale (rdale@ucla.edu) and Moreno I. Coco (moreno.cocoi@gmail.com)}

\references{
  
  Coco, M. I., & Dale, R. (2014). Cross-recurrence quantification 
  analysis of categorical and continuous time series: an R package. 
  Frontiers in psychology, 5, 510.
  
  
  Marwan, N., and Kurths, J. Nonlinear analysis of bivariate
  data with cross recurrence plots. Physics Letters A 302.5
  (2002): 299-307.

  Orsucci, F., Giuliani, A., Webber Jr, C., 
  Zbilut, J., Fonagy, P. and Mazza, M. (2006). 
  Combinatorics and synchronization in natural semiotics. 
  Physica A: Statistical Mechanics and its Applications, 361(2), 665-676.
  
  
  
}

\keyword{ package }

\examples{


txt = "here is a raw raw raw string, literally"
res = text_rqa(txt,typ = "string")
plot_rp(res$RP)


}
