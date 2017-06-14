#' Calculate grain-size mean (M1)
#' 
#' The function calculates the first moment of grain-size distributions, 
#' using either the arithmetic or geometric method.
#' 
#' @param data \code{Numeric} vector or matrix, grain-size 
#' distribution(s) to be evaluated.
#' 
#' @param classes \code{Numeric} vector, grain-size class limits 
#' corresponding to the input data set.
#' 
#' @param unit \code{Character} value, unit of the grainsize classes. One 
#' out of \code{"phi"} and \code{"mu"}, default is \code{"phi"}.
#' 
#' @param method \code{Character} value, method used for caluclation. One 
#' out of \code{"arithmetic"} and \code{"geometric"}, default is 
#' \code{"arithmetric"}.
#' 
#' @return \code{Numeric} vector, mean grain-size.
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## load example data set
#' data(LA950)
#' 
#' ## calculate arithemtric mean in metric scale
#' calc_mean(data = LA950$y, 
#'           classes = LA950$x, 
#'           unit = "mu")
#'           
#' ## calculate arithmetic mean in phi-scale
#' calc_mean(data = LA950$y, 
#'           classes = convert_units(mu = LA950$x), 
#'                                   unit = "phi")
#'                                   
#' ## calculate geometric mean
#' calc_mean(data = LA950$y, 
#'           classes = LA950$x, 
#'           unit = "mu", 
#'           method = "geometric")
#'                      
#' @export calc_mean
calc_mean <- function(
  data,
  classes,
  unit = "phi",
  method = "arithmetic"
) {
  
  ## homogenise data structure
  data <- rbind(data)
  data = as.list(as.data.frame(t(data)))
  
  ## normalise to constant sum
  data <- lapply(X = data, 
                 FUN = function(x) {
                   x / sum(x)
                 })
  
  ## calculate class mids
  classes_mid <- classes + c(diff(classes) / 2, 0)
  
  ## calculate arithmetic mean
  if(method == "arithmetic") {
    
    data_mean <- lapply(X = data, 
                        FUN = function(x, y) {
                          sum(x * y, na.rm = TRUE)
                        }, 
                        y = classes_mid)
    
  } else if(method == "geometric") {
    
    if(unit == "mu") {
      
      ## calculate geometric mean
      data_mean <- lapply(X = data, 
                          FUN = function(x, y) {
                            exp(sum(x * log(y), na.rm = TRUE))
                          }, 
                          y = classes_mid)
    } else if(unit == "phi") {
      
      ## calculate geometric mean
      data_mean <- lapply(X = data, FUN = function(x, y) {
        
        x_cum <- cumsum(x)
        
        sum(c(y[x_cum >= 0.16][1],
              y[x_cum >= 0.50][1],
              y[x_cum >= 0.84][1])) / 3
      }, 
      y = classes_mid)
      
    } else {
      
      ## print warning
      warning("calc_mean(): method keyword not supported")
      
      ## assign empty output
      data_mean <- rep(x = NA, times = length(data))
    }
    
    
  } else {
    
    ## print warning
    warning("calc_mean(): method keyword not supported")
    
    ## assign empty output
    data_mean <- rep(x = NA, times = length(data))
  }

    ## convert list to vector
  data_out <- as.numeric(unlist(data_mean))
  
  ## return output
  return(data_out)
}