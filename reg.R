g <- function(z) {
  return(1/(1+exp(-z)))
}


minmaxnorm <- function(x) {
  
  stopifnot(is.numeric(x))
  
  if (identical(range(x, na.rm = T)[1], range(x, na.rm = T)[2]))
    return(x/x)
  else
    return((x-min(x, na.rm=T)) / (max(x, na.rm=T) - min(x, na.rm=T)) )
}

zscorenorm <- function(x) {
  
  stopifnot(is.numeric(x))
  
  if (sd(x, na.rm = T) == 0)
    return(x/x)
  else
    return((x-mean(x, na.rm = T)) / sd(x, na.rm = T))
}


getcoef <- function(x, y) {
  
  xc <- x - mean(x)
  yc <- y - mean(y)
  xn <- xc / sd(x)
  yn <- yc / sd(y)
  return(coef(lm(yn ~ xn)))
  
  
  
}
  
