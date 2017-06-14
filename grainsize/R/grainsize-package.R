#' Grain-size data analysis toolbox
#'
#' Grain-size data analysis functions
#'
#' @name grainsize
#' @aliases grainsize
#' @docType package
#' @author Michael Dietze
#' @keywords grain-size
#' @importFrom stats approx spline
#' @importFrom utils read.table
NULL

#' CamSizerXT example data set
#' 
#' The dataset comprises a CamSizerXT measurement file.
#' 
#' @name CamSizerXT
#' @docType data
#' @format List of 4
#'          $ x   : num [1:256] 1 1.1 1.2 1.3 1.4 1.5 1.6 1.7 1.8 1.9 ...
#'          $ y   : num [1:256] 0.002 0 0.001 0 0 0 0.001 0 0 0 ...
#'          $ data:List of 4
#'           ..$ : num [1:256, 
#' @keywords datasets
#' @examples
#' 
# read example data set
#' data(CamSizerXT)
#' 
#' ## plot default data set parmeters (p3 vs. class limits)
#' plot(CamSizerXT, 
#'      type = "l", 
#'      log = "x")
#' 
"CamSizerXT"

#' LA950 example data set
#' 
#' The dataset comprises a LA950 laser sizer measurement file.
#' 
#' @name LA950
#' @docType data
#' @format List of 3
#'          $ x   : num [1:92] 0.02 0.023 0.026 0.03 0.034 0.039 ...
#'          $ y   : num [1:92] 0 0 0 0 0 0 0 0 0 0 ...
#'          $ meta:List of 24
#'           ..$ ID              : chr "S-1"
#'           ..$ data            : num [1:92] 0 0 0 0 0 0 0 0 0 0 ...
#'           ..$ unit            : chr "micrometres"
#' @keywords datasets
#' @examples
#' 
#' # read example data set
#' data(LA950, envir = environment())
#' 
#' # inspect imported data structure
#' str(LA950)
#' 
#' # plot grain-size distribution curve
#' plot(LA950, 
#'      type = "l", 
#'      log = "x")
#' 
"LA950"



