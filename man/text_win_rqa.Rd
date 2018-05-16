\name{text_win_rqa}
\alias{text_win_rqa}
\title{Window Recurrence quantification analysis ib text}
\description{
  It computes recurrence of text in overlapping windows over a number 
  of delays smaller than the size of the window. 
  }

\usage{
text_win_rqa(rsrc, typ = 'file', winsz = 10, wshft = 10, 
removeStopwords = F,embed = 1,tw = 1,limit = -1,shuffle = F)
}

\arguments{
  \item{rsrc}{Location of file or resource, or string literal}
    \item{typ}{A flag indicating the type of resource file in input: 
    typ = "file" (it's a file name);
    typ = "ulr"  (it's a url, and the file gets downloaded);
    typ = "string" or  "raw_chars" (it's a literal string);
    typ = "tibble" (it's a text formatted as tidytext by tibble)}
  \item{winsz}{The size of the window}
  \item{wshft}{Interval by which the window is moved.}
  \item{removeStopwords}{A boolean: TRUE (remove stop words) - FALSE (it retains them)}
   \item{embed}{The number of embedding dimension for
    phase-reconstruction, i.e., the lag intervals.}
    \item{tw}{The Theiler window parameter}
  \item{limit}{A scalar indicating how much text should be considered}   
  \item{shuffle}{A boolean if TRUE, it randomly shuffles the order of 
  the text for surrogate analyses.} 
}

\details{
 A wrapper to the `wincrqa()` function that runs windowed recurrence quantification
 analysis on text. This function also calls `get_text_series()` to 
 simplify the text in case such simplification was not done before
 inputting the text.
}

\value{

  It returns a matrix where the rows are the
  different windows explored, and the columns
  are the recurrence measures observed in that particular window.
  Refer to \code{text_win_rqa} for the values returned.


}

\author{Rick Dale (rdale@ucla.edu)}


\examples{

library(gutenbergr)
## let's get Alice's Adventures in Wonderland by Carroll
# gutenberg_works(author == "Carroll, Lewis") 
rsrc = gutenberg_download(11) ## take the text
win_rqa_res = text_win_rqa(rsrc, typ = "tibble", wshft = 150, winsz = 200, 
                       removeStopwords = FALSE, embed = 1, tw = 1,limit = -1,shuffle = FALSE)
win_rqa_res = as.data.frame(win_rqa_res$crqwin)

plot(win_rqa_res$window, win_rqa_res$RR)
abline(lm(win_rqa_res$RR ~ win_rqa_res$window), lwd = 3, col = "red")


}

\keyword{ts}
