#' Within-subjects coefficient of variation (Log method)
#'
#' Computation of percentage within subjects coefficient of variation given two vectors of measurements
#'
#' @param x The vector containing the first set of measurements.
#' @param y The vector containing the second set of measurements (must be the same lenght of x).
#'
#' @return The within-subject coefficient of variation according to QIBA.
#'
#' @author Giorgio Maria Agazzi, MD \email{giorgiomaria.agazzi@@gmail.com}
#' @seealso Google Scholar \url{https://scholar.google.com/citations?user=JuDwBQsAAAAJ&hl=it}
#' @keywords radiology repeatability
#'
#' @examples
#' first <- rnorm(20, mean = 10, sd = 1)
#' second <- rnorm(20, mean = 10, sd = 1)
#'
#' withincov(first, second)
#'
#'
#' @references 1. (2015) Quantitative Imaging Biomarkers: A Review of Statistical Methods for Technical Performance Assessment. Stat Methods Med Res 24:27–67. https://doi.org/10.1177/0962280214537344
#'
#' @export

withincovlog <- function(x, y) {
  
  #Get vector lenght
  N <- length(x)
  Ny <- length(y)
  
  #Control if vector lenght is equal
  if( N!=Ny ) stop('The two vectors must have the same lenght')
  
  #Log transform vectors
  x <- log(x)
  y <- log(y)
  
  #Initialize empty vector
  d <- NULL
  
  #Loop through the data
  for (i in 1:N) {
    
    d_temp <- (x[i] - y[i])^2
    
    d <- c(d, d_temp)
    
  }
  
  #Calculate within subjects sd
  d <- sqrt(sum(d)/2*N)
  
  #Calculate percentage Cov
  cov <- sqrt(exp(d^2)-1)
  
  #Return result
  return(cov)
  
}
