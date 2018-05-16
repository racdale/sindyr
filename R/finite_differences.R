#' Run Recurrence Quantification Analysis on text
#' 
#' @param rsrc location of file or resource, or string literal
#' @param typ specify whether 'file', 'url', or 'string'
#' @param removeStopwords omit closed-class words - 'stopwords'

.packageName <- 'crqanlp'

text_rqa = function(rsrc,typ='file',removeStopwords=F,embed=1,tw=1,limit=-1,shuffle=F) {
  ts = get_text_series(rsrc,typ=typ,removeStopwords=removeStopwords)
  if (limit>-1 & length(ts)>limit) {
    ts = ts[1:limit]
  }
  if (shuffle==T) {
    ts = sample(ts,length(ts))
  }
  rqa_res = crqa(ts,ts,delay=1,embed=embed,radius=.001,rescale=F,normalize=F,mindiagline=2,minvertline=2,tw=tw)
  return(rqa_res)
}
