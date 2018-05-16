\name{plot_rp}
\alias{plot_rp}
\title{Plot a recurrence matrix}
\description{
  Visualize a Recurrence Plot
  }

\usage{
plot_rp(RP,xlab='i',ylab='j',cex=.1)
}

\arguments{
  \item{RP}{A recurrence plot as produce by 'crqa()`}
  \item{xlab}{The name for the x axis} 
  \item{ylab}{The name for the y axis}   
  \item{cex}{The size of the recurrent point in the plot} 
}
\details{
 A function to plot recurrence matrices. 
}

\value{A plot of a recurrence matrix
}

\author{Rick Dale (rdale@ucla.edu)}


\examples{

txt = "here is a raw raw raw string, literally"
res = text_rqa(txt, typ = "string")
plot_rp(res$RP)


}

\keyword{misc}
