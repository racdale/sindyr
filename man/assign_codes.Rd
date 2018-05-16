\name{assign_codes}
\alias{assign_codes}
\title{Assign codes}
\description{
  It converts a sequence of words into a sequence of numerical 
  identifier. }

\usage{
assign_codes(words, wordCode)
}

\arguments{
  \item{words}{Vector of strings (tokens)}
  \item{wordCode}{vector of strings (types)}

}
\details{
  Imagine we want to treat all words from the semantic category
  of animals in the same way. This function would allow the 
  user to assign to different words, e.g, cat, dog or bird, 
  the same identifier, e.g., 1. Note, words and wordCode
  need to have the same length. 
}

\value{It returns the vector of words but now converted to a numerical 
  identifier, which corresponds to the associated wordCode.
}

\author{Rick Dale (rdale@ucla.edu)}


\examples{

library(corpus)
words    = c("bad", "bed", "joy", "sad") # words that we wish to locate in affect_wordnet
wordCode = affect_wordnet[,1]            # all words in affect_wordnet
assign_codes(words, wordCode)
 
}

\keyword{misc}
