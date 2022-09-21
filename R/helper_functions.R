
standardize_vector <- function(x){
  return((x-mean(x,na.rm=T))/sd(x,na.rm=T))
}

normalize_vector <- function(x){
  return((x-min(x,na.rm=T))/max(x,na.rm=T))
}

rhnorm <- function(n, sd = 1){
  abs(rnorm(n,sd = sd/sqrt(1-2/pi)))
}

