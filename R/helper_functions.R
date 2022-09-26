
#' Standardize and normalize vector
#'
#' Standardization and normalization of numeric vectors
#'
#' @param x Numeric vector to be standardized or normalized
#' @describeIn standardize_vector Standardizes vector by subtracting the mean and dividing by the standard deviation
#' @return Standardized vector
#' @export
#'
#' @examples x <- c(0,3,2,1,9,5,4,3,2,1)
#' standardize_vector(x)
standardize_vector <- function(x){
  if(sd(x)==0){
    warning('No variation in scores to standardize')
    return(x)
  }
  return((x-mean(x,na.rm=T))/sd(x,na.rm=T))
}

#' @describeIn standardize_vector Normalizes vector by subtracting the minimum value and dividing by the maximum value
normalize_vector <- function(x){
  if(sd(x)==0){
    warning('No variation in scores to normalize')
    return(x)
  }
  return((x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)))
}

#' Random draws from half normal distribution
#'
#' @param n Number of observations
#' @param sd standard deviation
#'
#' @return Vector of random draws from half normal distribution
#'
#' @examples rhnorm(10)
rhnorm <- function(n, sd = 1){
  abs(rnorm(n,sd = sd/sqrt(1-2/pi)))
}

rescale_ivs <- function(x,w){

}

