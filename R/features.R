#' Generate Features for SINDy Input
#' 
#' @param x raw data to be converted into features
#' @param polyorder order of polynomials (including k-th self products)
#' @param intercept include column of 1s in features to represent intercept (default = TRUE)
#' @return a new matrix of data with features from raw data

.packageName <- 'sindyr'

features = function(x,polyorder=2,intercept=TRUE) {
  nc = ncol(x)
  x = cbind(x,1)
  colnames(x) = c(colnames(x)[1:nc],"(Intercept)")

  combins = combinations(nc+1,k=polyorder,replace=T)
  x_temp = c()
  for (p in 1:nrow(combins)) { # loop through variable combinations (unique = combinations)
    colnm = paste(colnames(x)[combins[p,]],collapse=':') # get this column name, add interaction colon
    newcol = data.frame(rowProds(as.matrix(x),cols=combins[p,])) # create new column
    colnames(newcol) = colnm # set its column name
    if (length(x_temp)==0) {
      x_temp = newcol # if it's the first, set it as the starter data frame
    } else {
      x_temp = cbind(x_temp,newcol) # otherwise, append
    }
  }
  colnames(x_temp) = gsub('(\\:)\\(Intercept\\)','',colnames(x_temp)) # get rid of intercepts in product names (=1)
  colnames(x_temp) = gsub('\\(Intercept\\)(\\:)','',colnames(x_temp))
  x = x_temp
  if (!intercept) {
    x = x[,1:(ncol(x_temp)-1)] # remove the standalone intercept term... 
    sort.names.ix = sort(nchar(colnames(x)),index=T)$ix # sort names without intercept
  } else {
    sort.names.ix = c(ncol(x),sort(nchar(colnames(x)[1:(ncol(x)-1)]),index=T)$ix) # get intercept to front
  }
  
  x = x[,sort.names.ix]

  return(as.matrix(x)) # press the red button and run
}
