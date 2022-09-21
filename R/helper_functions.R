
standardize_vector <- function(x){
  return((x-mean(x,na.rm=T))/sd(x,na.rm=T))
}

normalize_vector <- function(x){
  return((x-min(x,na.rm=T))/max(x,na.rm=T))
}
