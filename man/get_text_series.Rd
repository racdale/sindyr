\name{get_text_series}
\alias{get_text_series}
\title{Get a text series}
\description{
  Acquire (and process) texts from different resources
}

\usage{
get_text_series(rsrc, typ='file',removeStopwords=F)
}

\arguments{
  \item{rsrc}{The resource text file}
  \item{typ}{A flag indicating the type of resource file in input: 
    typ = "file"" (it's a file name);
    typ = "ulr""  (it's a url, and the file gets downloaded)  = ;
    typ = "string | raw_chars" (it's a literal string);
    typ = "tibble" (it's a text formatted as tidytext by tibble)} 
  \item{removeStopwords}{A boolean: TRUE (remove stop words) - FALSE (it retains them)}

}

\details{
 A convenience function to obtain text from different types of source.
 It can be used to access text from the internet, from a file, 
 from a tibble text, or just from a literal string. 
 The user can also choose whether to remove stop words (or not). 
}

\value{It returns the vector of text already converted into a numerical identifier. 
}

\author{Rick Dale (rdale@ucla.edu)}


\examples{

## from a literal string
rsrc = "here is a raw raw raw string, literally"
ts = get_text_series(rsrc,typ='string')
print(ts)

## from tibble
library(gutenbergr)
## let's get Alice's Adventures in Wonderland by Carroll
# gutenberg_works(author == "Carroll, Lewis") 
rsrc = gutenberg_download(11) ## take the text
ts = get_text_series(rsrc, typ = "tibble", removeStopwords = TRUE)
print(ts[1:10])

## from URL
rsrc = "http://www.omegahat.net"
ts = get_text_series(rsrc, typ = "url")
print(ts[1:10])


}

\keyword{misc}
