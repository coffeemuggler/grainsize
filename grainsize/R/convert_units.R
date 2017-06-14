#' Convert between phi and micrometers.
#' 
#' The function converts values from the phi-scale to the 
#' micrometer-scale and vice versa.
#' 
#' @param phi \code{Numeric} vector, grain-size class values in phi.
#' 
#' @param mu \code{Numeric} vector, grain-size class values in micrometers.
#' 
#' @return \code{Numeric} vector, converted grain-size class values.
#' 
#' @author Michael Dietze
#' @keywords grainsize
#' @examples
#' 
#' ## generate phi-values
#' phi <- -2:5
#' 
#' ## convert and show phi to mu
#' mu  <- convert_units(phi = phi)
#' mu
#' 
#' ## convert and show phi to mu
#' convert_units(mu = mu)
#' 
#' @export convert_units
convert_units <- function(
  phi,
  
  mu
){
  
  if(missing(mu) == TRUE){

    ## convert phi to mu
    data_out <- 1000 * 2^-phi
    
  } else if(missing(phi) == TRUE){
    
    ## convert mu to phi
    data_out <- -log2(mu / 1000)
    
  } else {
    
    stop("convert_units(): No correct variables provided!")
  }
  
  ## return output data
  return(data_out)
}
