#' Within-subjects standard deviation (Log method)
#'
#' Computation of log transformed within subjects standard deviation given two vectors of measurements
#'
#' @param x The vector containing the first set of measurements.
#' @param y The vector containing the second set of measurements (must be the same lenght of x).
#'
#' @return The within-subject log transformed standard deviation according to QIBA.
#'
#' @author Giorgio Maria Agazzi, MD \email{giorgiomaria.agazzi@@gmail.com}
#' @seealso Google Scholar \url{https://scholar.google.com/citations?user=JuDwBQsAAAAJ&hl=it}
#' @keywords radiology repeatability
#'
#' @examples
#' first <- rnorm(20, mean = 10, sd = 1)
#' second <- rnorm(20, mean = 10, sd = 1)
#'
#' withinsdlog(first, second)
#'
#'
#' @references 1. (2015) Quantitative Imaging Biomarkers: A Review of Statistical Methods for Technical Performance Assessment. Stat Methods Med Res 24:27–67. https://doi.org/10.1177/0962280214537344
#'
#' @export

withinsdlog <- function(x, y) {
  
  #Get vector lenght
  N <- length(x)
  Ny <- length(y)
  
  #Add controlling if number of vectors is equal
  if( N!=Ny ) stop('The two vectors must have the same lenght')
  
  x <- log(x)
  y <- log(y)
  
  d <- NULL
  for (i in 1:N) {
    
    d_temp <- (x[i] - y[i])^2
    
    d <- c(d, d_temp)
    
  }
  
  d <- sqrt(sum(d)/2*N)
  
  #Return result
  return(d)
  
}
