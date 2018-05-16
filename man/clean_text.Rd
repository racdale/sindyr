\name{clean_text}
\alias{clean_text}
\title{Clean text}
\description{
  Pre-processing of raw text. It removes stop-words, punctuations,
  and create sentence markers.}

\usage{
clean_text(rawText,removeStopwords=F)
}

\arguments{
  \item{rawText}{A Vector of strings (tokens)}
  \item{removeStopwords}{A boolean: TRUE (remove stop words) - FALSE (it retains them)}

}
\details{
 A convenience function that removes unwanted information from a vector of text.
 The user has, at the moment, an argument to choose whether to remove stop words. 
}

\value{It returns the vector of text all in lower case, and stripped 
  from punctuations and stop-words. 
}

\author{Rick Dale (rdale@ucla.edu)}


\examples{

library(gutenbergr)
## let's get Alice's Adventures in Wonderland by Carroll
# gutenberg_works(author == "Carroll, Lewis") 
rawText = gutenberg_download(11) ## take the text
rawText = as.vector(rawText$text) ## vectorize the text
rawText = paste(rawText, collapse = " ") ## collapse the text

cleanText = clean_text(rawText, removeStopwords = TRUE)
text      = cleanText$content

}

\keyword{misc}
