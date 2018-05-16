#' Generate Features for SINDy Input
#' 
#' @param x raw data to be converted into features
#' @param polyorder order of polynomials (including k-th self products)
#' @return a new matrix of data with features from raw data

.packageName <- 'sindyr'

features = function(x,polyorder=2) {
  nc = ncol(x)
  if (polyorder>1) { # if polyorder is not greater than 1, then what are you doing here?
    for (i in 2:polyorder) { # let's make some snazzy names for our derived features
      x2 = x 
      colnames(x2) = paste(colnames(x),'_',i,sep='')
      x = cbind(x,x2) # make one for each order requested
    }
  }
  f = eval(parse(text=paste('~(.)^',polyorder,sep=''))) # now, let's get the full set of polyorder-th terms
  x_temp = model.matrix(f,x) # this will let us build the derived features
  x_nms = gsub('_\\d','',colnames(x_temp)) # we want to pass on the variable names here using _X as a search key
  uniq_nms = unique(x_nms) # this can produce some equivalences (e.g., xxy, yxx), so let's reduce
  for (i in 1:length(uniq_nms)) { # for all unique ones, let's get some nice names
    uniq_nms[i] = paste(sort(unlist(strsplit(uniq_nms[i],':'))),collapse=':') # rebuild interaction term names with :
  }
  uniq_nms = unique(uniq_nms) # let's reduce again looking for repeats
  x = c()
  for (n in uniq_nms) { # okay, let's start to build the materials for the party, based on what unique items remain
    x = cbind(x,x_temp[,which(x_nms==n)[1]])
  }
  colnames(x) = uniq_nms # now we have the unique names
  return(x) # press the red button and run
}
