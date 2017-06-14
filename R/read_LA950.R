#' Read LA-950 measurement files.
#' 
#' This function reads the ASCII output files to R. 
#' 
#' @param file \code{Character} value, name of the file to be read, 
#' with extension.
#' 
#' @param version \code{Character} value, file format version. Default is
#' \code{"8"}.
#' 
#' @param normalise \code{Logical} value, option to normalise measured 
#' class values to the base of 100 percent. Default is \code{TRUE}.
#' 
#' @return \code{list} object containing sample ID, grain-size data
#' and metadata.
#' 
#' @author Michael Dietze
#' @keywords grainsize
#' @examples
#' 
#' ## uncomment to use or use example data set just below
#' # ## load example data
#' # x <- read.LA950(file = "LA950.txt")
#' 
#' ## read example data set
#' data(LA950)
#' 
#' # inspect imported data structure
#' str(LA950)
#' 
#' # plot grain-size distribution curve
#' plot(LA950, type = "l", log = "x")
#' 
#' # import all files in a folder and write data to a matrix
#' #   X <- numeric(0)
#' #   ID <- numeric(0)
#' #   files <- list.files()
#' #   for(i in 1:length(files)) {
#' #     x <- read.LA950(file = files[i])
#' #     X <- rbind(X, x$y)
#' #     ID <- c(ID, x$meta$ID)
#' #   }
#' #   colnames(X) <- x$x
#' #   rownames(X) <- ID  
#' 
#' @export read_LA950
read_LA950 <- function(
  file,
  version = "8",
  normalise = TRUE
) {
  #############################################################################
  ## ATTENTION, FRAUNOFER OPTION NOT IMPLEMENTED AND, THUS, NOT TESTED       ##
  #############################################################################
  
  ## check if file exists
  if(file.exists(file) == FALSE) {
    
    stop("File does not exist!")
  }
  
  ## read file
  x <- readLines(con = file, 
                 warn = FALSE)
  
  ## replace invalid strings
  x <- gsub(x = x, 
            pattern = "(\xb5m)",
            replacement = "micrometres")
  
  ## assign ID
  i_sample_name <- grepl(pattern = "Sample Name", 
                         x = x, 
                         perl = TRUE)
  
  ID <- strsplit(x = x[i_sample_name][1], 
                 split = "\t")[[1]][2]
  
  ## assign transmittance(R)
  if(version == "8") {
    
    i_transmittanceR <- grepl(pattern = "Transmittance (R)", 
                              x = x, 
                              fixed = TRUE)
  } else {
    
    i_transmittanceR <- grepl(pattern = "Transmittance(R)", 
                              x = x, 
                              fixed = TRUE)
  }
  
  TrR <- strsplit(x = x[i_transmittanceR], 
                  split = "\t")[[1]][2]
  
  TrR <- as.numeric(strsplit(x = TrR, 
                             split = "\\(")[[1]][1])
  
  ## assign Transmittance(B)
  if(version == "8") {
    
    i_transmittanceB <- grepl(pattern = "Transmittance (B)", 
                              x = x, 
                              fixed = TRUE)
  } else {
    
    i_transmittanceB <- grepl(pattern = "Transmittance(B)", 
                              x = x, 
                              fixed = TRUE)
  }
  
  TrB <- strsplit(x = x[i_transmittanceB], 
                  split = "\t")[[1]][2]
  
  TrB <- as.numeric(strsplit(x = TrB, 
                             split = "\\(")[[1]][1])
  
  ## assign Ultra Sound
  if(version == "8") {
    
    i_US <- grepl(pattern = "Ultrasound", 
                  x = x, 
                  fixed = TRUE)
  } else {
    
    i_US <- grepl(pattern = "Ultra Sonic ", 
                  x = x, 
                  fixed = TRUE)
  }
  
  US <- strsplit(x = x[i_US], 
                 split = "\t")[[1]][2]
  
  if(US == "Off") {
    
    US <- 0
  } else {
    
    US <- strsplit(x = US, 
                   split = "\\(")
    US <- as.POSIXlt(x = US[[1]][1], 
                     format = "%M:%S")
    US <- US$min * 60 + US$sec
  }
  
  ## assign Circulation Speed
  if(version == "8") {
    
    i_CS <- grepl(pattern = "Circulation speed", 
                  x = x, 
                  fixed = TRUE)
  } else {
    
    i_CS <- grepl(pattern = "Circulation Speed", 
                  x = x, 
                  fixed = TRUE)
  }
  
  CS <- as.numeric(strsplit(x = x[i_CS], 
                            split = "\t")[[1]][2])
  
  ## assign Agitation Speed
  if(version == "8") {
    
    i_AS <- grepl(pattern = "Agitation speed", 
                  x = x, 
                  fixed = TRUE)
  } else {
    
    i_AS <- grepl(pattern = "Agitation Speed", 
                  x = x, 
                  fixed = TRUE)
  }
  
  AS <- as.numeric(strsplit(x = x[i_AS], 
                            split = "\t")[[1]][2])
  
  ## assign iteration mode
  if(version == "8") {
    
    i_IM <- grepl(pattern = "Iteration mode", 
                  x = x, 
                  fixed = TRUE)
    
    IM <- as.character(strsplit(x = x[i_IM], 
                                split = "\t")[[1]][2])
  } else {
    
    i_IM <- grepl(pattern = "Form of Distribution", 
                  x = x, 
                  fixed = TRUE)
    
    IM <- as.character(strsplit(x = x[i_IM], 
                                split = "\t")[[1]][2])
  }
  
  ## assign Distribution Base
  if(version == "8") {
    
    i_DB <- grepl(pattern = "Distribution base", 
                  x = x, 
                  fixed = TRUE)
  } else {
    
    i_DB <- grepl(pattern = "Distribution Base", 
                  x = x, 
                  fixed = TRUE)
  }
  
  DB <- as.character(strsplit(x = x[i_DB], 
                              split = "\t")[[1]][2])
  
  ## assign Refractive Index (R)
  if(version == "8") {
    
    i_RiAR <- grepl(pattern = "Refractive index (R)", 
                    x = x, 
                    fixed = TRUE)
    
    RiAR <- strsplit(x = x[i_RiAR], 
                     split = "\t")[[1]][2]
    
    if(RiAR[1] == "Fraunhofer Kernel") {
      
      RiR <- complex(0)
      AbR <- numeric(0)
      
      model <- "Fraunhofer"
    } else {
      
      RiAR <- strsplit(x = RiAR, 
                       split = "(", 
                       fixed = TRUE)[[1]]
      
      RiR <- as.complex(RiAR[1])
      AbR <- as.numeric(strsplit(x = RiAR[2], 
                                 split = ")", 
                                 fixed = TRUE)[[1]][1])
      
      model <- "Mie"
    }
  } else {
    
    RiAR <- seq(from = 1, 
                to = length(x))[grepl(pattern = "Refractive Index (R)", 
                                      x = x, 
                                      fixed = TRUE)]
    RiAR <- strsplit(x = x[RiAR], 
                     split = "\t")
    
    if(grepl(pattern = "Fraunhofer", 
             x = RiAR[[1]][2]) == TRUE) {
      
      RiR <- complex(0)
      AbR <- numeric(0)
      model <- "Fraunhofer Kernel"
      
    } else {
      RiAR <- strsplit(x = RiAR[[1]][2], 
                       split = "\\[")
      RiAR <- strsplit(x = RiAR[[1]][1], 
                       split = "\\(")
      RiR <- as.complex(RiAR[[1]][1])
      AbR <- strsplit(x = RiAR[[1]][2], 
                      split = "\\)")
      AbR <- as.numeric(AbR[[1]][1])
      model <- "Mie"
    }
  }
  
  ## assign Refractive Index (B)
  if(version == "8") {
    
    i_RiAB <- grepl(pattern = "Refractive index (B)", 
                    x = x, 
                    fixed = TRUE)
    
    RiAB <- strsplit(x = x[i_RiAB], 
                     split = "\t")[[1]][2]
    
    if(RiAB == "Fraunhofer Kernel") {
      
      RiB <- complex(0)
      AbB <- numeric(0)
      
      model <- "Fraunhofer"
    } else {
      
      RiAB <- strsplit(x = RiAB, 
                       split = "(", 
                       fixed = TRUE)[[1]]
      
      RiB <- as.complex(RiAB[1])
      AbB <- as.numeric(strsplit(x = RiAB[2], 
                                 split = ")", 
                                 fixed = TRUE)[[1]][1])
      
      model <- "Mie"
    }

  } else {
    
    RiAB <- seq(from = 1, 
                to = length(x))[grepl(pattern = "Refractive Index (R)",
                                      x = x, 
                                      fixed = TRUE)]
    RiAB <- strsplit(x = x[RiAB], 
                     split = "\t")
    if(grepl(pattern = "Fraunhofer", 
             x = RiAB[[1]][2]) == TRUE) {
      RiB <- complex(0)
      AbB <- numeric(0)
      model <- "Fraunhofer Kernel"
    } else {
      RiAB <- strsplit(x = RiAB[[1]][2], 
                       split = "\\[")
      RiAB <- strsplit(x = RiAB[[1]][1], 
                       split = "\\(")
      RiB <- as.complex(RiAB[[1]][1])
      AbB <- strsplit(x = RiAB[[1]][2], 
                      split = "\\)")
      AbB <- as.numeric(AbB[[1]][1])
      model <- "Mie"
    }
  }
  
  ## assign Material
  i_material <- grepl(pattern = "Material", 
                      x = x, 
                      perl = TRUE)
  
  Mat <- strsplit(x = x[i_material][1], 
                  split = "\t")[[1]][2]
  
  ## assign Source
  i_Source <- grepl(pattern = "Source", 
                    x = x, 
                    perl = TRUE)
  
  Source <- strsplit(x = x[i_Source][1], 
                     split = "\t")[[1]][2]
  
  ## assign Lot Number
  i_Lot <- grepl(pattern = "Lot Number", 
                 x = x, 
                 perl = TRUE)
  
  Lot <- strsplit(x = x[i_Lot][1], 
                  split = "\t")[[1]][2]
  
  ## assign Test or Assay. Number
  if(version == "8") {
    
    TAN <- NA
  } else {
    
    i_TAN <- grepl(pattern = "Test or Assay. Number", 
                   x = x, 
                   perl = TRUE)
    
    TAN <- strsplit(x = x[i_TAN][1], 
                    split = "\t")[[1]][2]
  }
  
  ## assign Sample Data Acquisition Times (LD)
  if(version == "8") {
    
    i_AtR <- grepl(pattern = "Sample data acquisition times (R)", 
                   x = x, 
                   fixed = TRUE)
  } else {
    
    i_AtR <- grepl(pattern = "Sample Data Acquisition Times (LD)", 
                   x = x, 
                   fixed = TRUE)
  }
  
  AtR <- as.numeric(strsplit(x = x[i_AtR], 
                             split = "\t")[[1]][2])
  
  ## assign Sample Data Acquisition Times (LED)
  if(version == "8") {
    
    i_AtB <- grepl(pattern = "Sample data acquisition times (B)", 
                   x = x, 
                   fixed = TRUE)
  } else {
    
    i_AtB <- grepl(pattern = "Sample Data Acquisition Times (LED)", 
                   x = x, 
                   fixed = TRUE)
  }
  
  AtB <- as.numeric(strsplit(x = x[i_AtB], 
                             split = "\t")[[1]][2])
  
  ## assign Result File Name
  if(version == "8") {
    
    i_FN <- grepl(pattern = "Result file name", 
                  x = x, 
                  fixed = TRUE)
  } else {
    
    i_FN <- grepl(pattern = "Result File Name ", 
                  x = x, 
                  fixed = TRUE)
  }
  
  FN <- as.character(strsplit(x = x[i_FN], 
                              split = "\t")[[1]][2])
  
  ## assign Data Name
  if(version == "8") {
    
    i_DN <- grepl(pattern = "Data name", 
                  x = x, 
                  fixed = TRUE)
  } else {
    
    i_DN <- grepl(pattern = "Data Name", 
                  x = x, 
                  fixed = TRUE)
  }
  
  DN <- as.character(strsplit(x = x[i_DN], 
                              split = "\t")[[1]][2])
  
  ## assign Algorithm Option
  if(version == "8") {
    
    i_AO <- grepl(pattern = "Algorithm", 
                  x = x, 
                  fixed = TRUE)
  } else {
    
    i_AO <- grepl(pattern = "Algorithm Option", 
                  x = x, 
                  fixed = TRUE)
  }
  
  AO <- as.character(strsplit(x = x[i_AO], 
                              split = "\t")[[1]][2])
  
  ## assign Unit
  if(version == "8") {
    
    i_Unit <- grepl(pattern = "\tq3*", 
                    x = x, 
                    perl = TRUE)
    
    Unit <- strsplit(x = as.character(x[i_Unit]), 
                     split = "\t", 
                     perl = TRUE)[[1]][1]
    
    Unit <- strsplit(x = Unit, 
                     split = "X(", 
                     fixed = TRUE)[[1]][2]
    
    Unit <- strsplit(x = Unit, 
                     split = ")", 
                     fixed = TRUE)[[1]][1]
    
  } else {
    
    Unit <- seq(from = 1, 
                to = length(x))[grepl(pattern = "Diameter", 
                                      x = x, 
                                      perl = TRUE)][2]
    Unit <- strsplit(x = as.character(x[Unit]), 
                     split = "\t", perl = TRUE)
    Unit <- strsplit(x = Unit[[1]][1], 
                     split = "\\(")
    Unit <- strsplit(x = Unit[[1]][2], 
                     split = "\\)")
    Unit <- Unit[[1]][1]
  }
  
  ## identify start and end line for data extraction
  if(version == "8") {
    
    line_0 <- grepl(pattern = "\tq3*", 
                    x = x, 
                    fixed = TRUE)
    
    results <- suppressWarnings(read.table(file = file, 
                                           header = FALSE, 
                                           skip = seq(from = 1, 
                                                      to = length(x))[line_0]))
    
    if(normalise == TRUE) {
      
      results[,2] <- results[,2] / sum(results[,2]) * 100
    }
    
    names(results) <- c("diameter", 
                        "amount", 
                        "cumulative")
    
  } else {
    
    ## identify start and end line for data extraction
    line.0 <- seq(from = 1, 
                  to = length(x))[grepl(pattern = "Diameter", 
                                        x = x, 
                                        perl = TRUE)][2] + 1
    line.n <- seq(from = 1, 
                  to = length(x))[grepl(pattern="2500.00", 
                                        x = x, 
                                        perl = TRUE)]
    
    ## extract and prepare measurement data
    results <- x[line.0:line.n]
    results <- strsplit(x = results, 
                        split = "\t")
    results <- t(matrix(data = as.numeric(unlist(results)), 
                        nrow = 3))
    colnames(results) <- c("diameter", 
                           "amount", 
                           "cumulative")
  }
  
  ##create data structure
  data <- list(x = results[,1],
               y = results[,2],
               meta = list(
                 ID = ID,
                 data = results,
                 unit = Unit,
                 material = Mat,
                 source = Source,
                 model = model,
                 circ.speed = CS,
                 agit.speed = AS,
                 ultrasonic = US,
                 dist.form = IM,
                 dist.base = DB,
                 trans.R = TrR,
                 trans.B = TrB,
                 Ri.R = RiR,
                 Ab.R = AbR,
                 Ri.B = RiB,
                 Ab.B = AbB,
                 acquisit.times.R = AtR,
                 acquisit.times.B = AtB,
                 filename = FN,
                 dataname = DN,
                 algorithm = AO,
                 lot.nr = Lot,
                 testassay.nr = TAN))
  
  return(data)
}