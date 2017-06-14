#' Interpolate data between different classes.
#' 
#' Interpolate grain-size data for different classes, either to 
#' higher or to lower resolution.
#' 
#' @param data \code{Numeric} matrix, input data set with m samples (rows) 
#' and n variables (columns).
#'  
#' @param boundaries_in \code{Numeric} vector, class boundaries of the 
#' input data.
#' 
#' @param boundaries_out \code{Numeric} vector, class boundaries of the output
#' data.
#' 
#' @param method \code{Logical} scalar, interpolation method, one out of 
#' "linear" (linear interpolation), "fmm" (cubic spline), "natural" 
#' (natural spline), "periodic" (periodic spline). Default is \code{"natural"}.
#' 
#' @param fixed \code{Logical} value, specifying if the outer boundaries 
#' should be set to the same values as in the original matrix, default is 
#' \code{TRUE}. This may become necessary to avoid interpolation errors, see 
#' example.
#' 
#' @return \code{Numeric} matrix, interpolated class values.
#' 
#' @author Michael Dietze
#' @keywords grainsize
#' @examples
#' 
#' ## load example data
#' data(LA950, envir = environment())
#' 
#' ## convert units to phi-scale
#' classes_in <- convert_units(mu = LA950$x)
#' 
#' ## Example 1 - decrease the class numbers
#' ## define number of output classes
#' classes_out <- seq(from = max(classes_in), 
#'                    to = min(classes_in), 
#'                    length.out = 20)
#'                    
#' ## interpolate the data set
#' Y <- interpolate_classes(data = LA950$y, 
#'                          boundaries_in = classes_in, 
#'                          boundaries_out = classes_out,
#'                          method = "linear")
#'                          
#' ## show original vs. interpolation for first 10 samples
#' plot(NA, xlim = range(classes_in), ylim = range(LA950$y))
#' lines(classes_in, LA950$y)
#' lines(classes_out, Y, col = 2)
#' 
#' ## Example 2 - increase the class numbers
#' ## define number of output classes
#' classes_out <- seq(from = max(classes_in), 
#'                    to = min(classes_in), 
#'                    length.out = 200)
#' 
#' ## interpolate the data set
#' Y <- interpolate_classes(data = LA950$y, 
#'                          boundaries_in = classes_in, 
#'                          boundaries_out = classes_out,
#'                          method = "linear")
#' 
#' ## show original vs. interpolation for first 10 samples
#' plot(NA, xlim = range(classes_in), ylim = range(LA950$y))
#' lines(classes_in, LA950$y)
#' lines(classes_out, Y, col = 2)
#' 
#' @export interpolate_classes
interpolate_classes <- function(
  data,
  boundaries_in,
  boundaries_out,
  method = "natural",
  fixed = TRUE
){
  
  ## check input data
  if(min(boundaries_in, na.rm = TRUE) > 
     min(boundaries_out, na.rm = TRUE) |
     max(boundaries_in, na.rm = TRUE) < 
     max(boundaries_out, na.rm = TRUE)) {
    
    stop("interpolate_classes(): Output is wider than input boundary range!")
  }
  
  ## transform data structure of X
  if(is.matrix(data) == FALSE) {
    
    data <- t(as.matrix(data))
  }
  
  ## create cumulative class sums matrix
  Xcum <- t(apply(data, 1, cumsum))
  
  ## create output matrices
  Y <- matrix(nrow = nrow(data), ncol = length(boundaries_out))
  Ycum <- Y
  
  if(method == "linear") {
    ## linear interpolation
    for(i in 1:nrow(data)) {
      Ycum[i,] <- approx(x = boundaries_in,
                         y = Xcum[i,],
                         xout = boundaries_out,
                         rule = 1)$y
    }
  } else {
    
    ## spline interpolation
    for(i in 1:nrow(data)) {
      Ycum[i,] <- spline(x = boundaries_in, 
                         y = Xcum[i,], 
                         xout = boundaries_out, 
                         method = method)$y
    }
  }
  
  ## reverse cumulative sums to individual class sums
  Y[,1] <- Ycum[,1]
  Y[,2:ncol(Y)] <- Ycum[,2:ncol(Ycum)] - Ycum[,1:(ncol(Ycum) - 1)]
  
  ## rescale interpolated data to original limits
  Y.norm <- Y
  Y.scaled <- Y
  for(i in 1:nrow(data)){
    Y.norm[i,] <- (Y[i,] - min(Y[i,])) / max(Y[i,] - min(Y[i,]))
    Y.scaled[i,] <- Y.norm[i,] * (max(data[i,]) - min(data[i,])) + min(data[i,])
  }
  
  ## optionally, assign original values to first class
  if(fixed == TRUE) {
    Y.scaled[,1] <- data[,1]
  }
  
  ## return result matrix
  return(Y.scaled)
}