\name{text_rqa}
\alias{text_rqa}
\title{Recurrence quantification analysis on categorical series of text}
\description{
  Compute recurrence quantification on text.
  }

\usage{
text_rqa(rsrc,typ = 'file',removeStopwords = F,embed = 1,tw = 1,limit = -1,shuffle = F)
}

\arguments{
  \item{rsrc}{Location of file or resource, or string literal}
    \item{typ}{A flag indicating the type of resource file in input: 
    typ = "file" (it's a file name);
    typ = "ulr"  (it's a url, and the file gets downloaded);
    typ = "string" or  "raw_chars" (it's a literal string);
    typ = "tibble" (it's a text formatted as tidytext by tibble)} 
  \item{removeStopwords}{A boolean: TRUE (remove stop words) - FALSE (it retains them)}
   \item{embed}{The number of embedding dimension for
    phase-reconstruction, i.e., the lag intervals.}
    \item{tw}{The Theiler window parameter}
  \item{limit}{A scalar indicating how much text should be considered for the analysis}   
  \item{shuffle}{A boolean: if TRUE, it randomly shuffles the order of 
  the text for surrogate analyses.} 
}

\details{
 A wrapper to the `crqa()` function that runs recurrence quantification
 analysis on text. This function also calls `get_text_series()` to 
 simplify the text in case such simplification was not done before
 inputting the text.
}

\value{

  It returns a list with different measures extracted from the recurrence
  plot. Otherwise, the values for the output arguments will be either 0 or NA.
  
  \item{RR}{The percentage of recurrent points falling within
    the specified radius (range between 0 and 100)}
  \item{DET}{Proportion of recurrent points forming diagonal
    line structures.}
  \item{NRLINE}{The total number of lines in the recurrent plot}
  \item{maxL}{The length of the longest diagonal line
    segment in the plot, excluding the main diagonal}
  \item{L}{The average length of line structures}
  \item{ENTR}{Shannon information entropy of
    diagonal line lengths longer than the minimum length}
  \item{rENTR}{Entropy measure normalized by the number of
    lines observed in the plot. Handy to compare across contexts
    and conditions}
  \item{LAM}{Proportion of recurrent points forming vertical
    line structures}
  \item{TT}{The average length of vertical line structures}


}

\author{Rick Dale (rdale@ucla.edu)}


\examples{

txt = "here is a raw raw raw string, literally"
res = text_rqa(txt,typ = "string")
plot_rp(res$RP)


}

\keyword{ts}
