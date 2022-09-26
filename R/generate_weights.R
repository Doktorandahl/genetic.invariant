


#' Generate weights for genetic algorithm
#'
#' @param n Number of weights to be generated
#' @param dist Distribution to generate weights from. Either a vector of allowed weights to sample from, OR the name of a probability distribution to sample from. E.g. 'hnorm' for the half normal distribution or 'unif' for the uniform distribution
#' @param dot_args Additional arguments to be passed to the random number generating function if used.
#'
#' @return Returns a vector of weights
#' @noRd
#'
#' @keywords internal
gen_wts <- function(n,dist = "hnorm",dot_args=NULL){
if(is.numeric(dist)){
  return(sample(dist,n,replace=T))
}else{
  do.call(match.fun(paste('r',dist,sep="")),args = c(list(n),dot_args))
}

}
