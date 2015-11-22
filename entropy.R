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

#' Function to calculate Information Gain/Deviance
#' 
#' Calculate Information Gain or Deviance based on supplied previously 
#'  calculated entropy and a numeric vector representing the probability of 
#'  each partitioned attribute value.
#' @param p numeric vector, each element indicates the corresponding probability
#' @param e numeric value of previously calcuated entropy, default is 1
#' @return a numeric value, the information gain, higher means more info, better 
#'  classifier
#' @author Siyu Sun (sunsiyu.tud@gmail.com)
#' @examples 
#' e <- getinfogain(p=c(0.1, 0.9), e=1)
getinfogain <- function(p, e=1, deviance = F) 
{
  stopifnot((!is.null(p)) && (!is.null(e)) )
  stopifnot(is.numeric(p) && is.numeric(e))
  if (sum(p) != 1)
    stop("The sum of all probability should be equal to 1!")
  
  p_zero <- which(p == 0)
  p_rest <- p[-p_zero]
  
  if (length(p_rest) < 1)
    return (e - numeric(0))
  
  if (deviance)
    return (e - sum(-p_rest * log(p_rest)))
  else
    return (e - sum(-p_rest * log2(p_rest)))

}