#' Generate a sequence of numeric identifiers
#' 
#' @param rsrc location of file or resource, or string literal
#' @param typ specify whether 'file', 'url', or 'string'
#' @param removeStopwords omit closed-class words - 'stopwords'

.packageName <- 'crqanlp'

text_win_rqa = function(rsrc,typ='file',winsz = 10 ,wshft = 10,removeStopwords=F,embed=1,tw=1,limit=-1,shuffle=F) {
  ts = get_text_series(rsrc,typ=typ,removeStopwords=removeStopwords)
  #rqa_res = crqa(ts,ts,delay=1,embed=embed,radius=.001,rescale=F,normalize=F,mindiagline=2,minvertline=2,tw=tw)
  if (limit>-1 & length(ts)>limit) {
    ts = ts[1:limit]
  }
  if (shuffle==T) {
    ts = sample(ts,length(ts))
  }
  if (length(ts)<winsz) {
    print('Time series is too short given window parameters.')
    rqa_res = data.frame(crqwin=-1,TREND=-1)
    return(rqa_res)
  }
  rqa_res = wincrqa(ts,ts,windowstep=wshft,windowsize=winsz,delay=1,
               embed=embed,rescale=0,radius=.001,normalize=0,
               mindiagline=2,minvertline=2,tw=tw,whiteline=F,trend=F)
  colnames(rqa_res$crqwin) = c('RR','DET','NRLINE','maxL','L','ENTR','rENTR','LAM','TT','window')
  return(rqa_res)
}
