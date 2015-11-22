#' Function to calculate Entropy
#' 
#' Calculate Entropy based on supplied vector of probability.
#' @param p numeric vector, each element indicates the corresponding probability
#' @return a numeric value, the entropy
#' @author Siyu Sun (sunsiyu.tud@gmail.com)
#' @examples 
#' e <- getentropy(c(0.1, 0.9))
getentropy <- function(p) 
{
  stopifnot(!is.null(p))
  stopifnot(is.numeric(p))
  if (sum(p) != 1)
    stop("The sum of all probability should be equal to 1!")
  
  p_zero <- which(p == 0)
  p_rest <- p[-p_zero]
  
  if (length(p_rest) < 1)
    return (numeric(0))
  else
    return (sum(-p_rest * log2(p_rest)))
}
  